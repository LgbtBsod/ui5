#!/usr/bin/env node
const path = require('path');
const {
  ROOT,
  listRuntimeJsFiles,
  parseImports,
  readAbsolute,
  writeDoc,
  writeDocJson
} = require('./lib/architectureArtifactsShared');

function longFunctionCount(text) {
  const lines = text.split('\n');
  let count = 0;
  let start = -1;
  for (let i = 0; i < lines.length; i++) {
    if (lines[i].match(/function\s+[A-Za-z0-9_]+\s*\(|=>\s*\{/)) start = i;
    if (start >= 0 && lines[i].includes('}')) {
      if (i - start + 1 > 60) count++;
      start = -1;
    }
  }
  return count;
}

function backendSignatures(text) {
  const sigs = [];
  const readRe = /(\.read\(|\.callFunction\(|\.create\(|\.update\()\s*['"]([^'"]+)['"]/g;
  let m;
  while ((m = readRe.exec(text))) sigs.push(`${m[1]} ${m[2]}`);
  return sigs;
}

function main() {
  const files = listRuntimeJsFiles();
  const graph = {};
  const violations = [];
  const features = new Map();
  const backend = new Map();
  const dupes = new Map();
  const hotspots = [];
  const forbiddenController = ['infra/adapters', 'infra/odata', 'service/backend'];

  for (const file of files) {
    const f = file;
    const text = readAbsolute(path.join(ROOT, file));
    const imports = parseImports(text);
    graph[f] = imports;

    if (f.startsWith('controller/')) {
      for (const imp of imports) {
        if (forbiddenController.some((x) => imp.includes(x))) {
          violations.push({ file: f, rule: 'controller-forbidden-import', import: imp });
        }
      }
    }
    if (f.startsWith('service/runtime/') && text.includes('.setProperty(')) {
      violations.push({ file: f, rule: 'manager-jsonmodel-write', import: '.setProperty(' });
    }

    const t = f.toLowerCase();
    const assignFeature = (k) => {
      if (!features.has(k)) features.set(k, []);
      features.get(k).push(f);
    };
    if (t.includes('search')) assignFeature('Search');
    if (t.includes('detail')) assignFeature('Detail');
    if (t.includes('lock')) assignFeature('Lock lifecycle');
    if (t.includes('attach')) assignFeature('Attachments');
    if (t.includes('dict') || t.includes('valuehelp')) assignFeature('Dictionary/value help');
    if (t.includes('person') || t.includes('suggest')) assignFeature('Person suggest');
    if (t.includes('error') || t.includes('effect') || t.includes('feedback')) assignFeature('Error normalization + feedback');

    for (const s of backendSignatures(text)) {
      if (!backend.has(s)) backend.set(s, []);
      backend.get(s).push(f);
    }

    const lineCount = text.split('\n').length;
    const fanOut = imports.length;
    const longFns = longFunctionCount(text);
    const dCount = (text.match(/\.read\(|\.callFunction\(/g) || []).length;
    hotspots.push({ file: f, score: lineCount * fanOut + longFns * 50 + dCount * 30, lineCount, fanOut });

    const fingerprints = text.split('\n').map((l) => l.trim()).filter(Boolean);
    for (let i = 0; i + 2 < fingerprints.length; i++) {
      const fp = [fingerprints[i], fingerprints[i + 1], fingerprints[i + 2]].join(' | ');
      if (fp.length < 80) continue;
      if (!dupes.has(fp)) dupes.set(fp, new Set());
      dupes.get(fp).add(f);
    }
  }

  hotspots.sort((a, b) => b.score - a.score);

  writeDocJson('dependency-graph.json', graph);
  writeDocJson('layer-violations.json', violations);

  const featText = ['# Feature map', ''];
  for (const name of ['Search', 'Detail', 'Lock lifecycle', 'Attachments', 'Dictionary/value help', 'Person suggest', 'Error normalization + feedback']) {
    featText.push(`## ${name}`);
    const owned = (features.get(name) || []).slice(0, 20).map((f) => `- ${f}`);
    featText.push(...(owned.length ? owned : ['- (no direct owner detected)']), '');
  }
  writeDoc('feature-map.md', featText.join('\n'));

  const hotText = ['# Hotspots', '', '| file | score | lines | fanOut |', '|---|---:|---:|---:|'];
  hotspots.slice(0, 30).forEach((h) => hotText.push(`| ${h.file} | ${h.score} | ${h.lineCount} | ${h.fanOut} |`));
  writeDoc('hotspots.md', hotText.join('\n'));

  const dupText = ['# Duplication report', ''];
  for (const [fp, set] of [...dupes.entries()].filter(([, s]) => s.size > 1).slice(0, 50)) {
    dupText.push(`- cluster (${set.size} files): ${[...set].join(', ')}`);
    dupText.push(`  - fingerprint: ${fp.slice(0, 180)}...`);
  }
  writeDoc('duplication-report.md', dupText.join('\n'));

  const bText = ['# Backend call inventory', ''];
  for (const [sig, owners] of backend.entries()) {
    bText.push(`- ${sig}`);
    owners.forEach((o) => bText.push(`  - ${o}`));
  }
  writeDoc('backend-call-inventory.md', bText.join('\n'));

  console.log('Generated architecture artifacts.');
}

main();
