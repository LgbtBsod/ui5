#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

function walk(dir, out) {
  fs.readdirSync(dir, { withFileTypes: true }).forEach((e) => {
    const p = path.join(dir, e.name);
    if (e.isDirectory()) return walk(p, out);
    if (e.isFile() && p.endsWith('.xml')) out.push(p);
  });
}

function collectControls(xml) {
  const m = xml.match(/<([a-zA-Z0-9_.:-]+)(\s|>|\/)/g) || [];
  const counts = {};
  m.forEach((token) => {
    const tag = token.replace(/[<\s>/]/g, '');
    if (!tag || tag.startsWith('mvc:') || tag.startsWith('core:FragmentDefinition')) return;
    counts[tag] = (counts[tag] || 0) + 1;
  });
  return Object.entries(counts)
    .sort((a, b) => b[1] - a[1])
    .slice(0, 12)
    .map(([tag, count]) => ({ tag, count }));
}

const root = process.cwd();
const viewDir = path.join(root, 'view');
const files = [];
walk(viewDir, files);
files.sort();

const report = {
  generatedAt: new Date().toISOString(),
  fileCount: files.length,
  entities: files.map((f) => {
    const rel = path.relative(root, f);
    const content = fs.readFileSync(f, 'utf8');
    const type = rel.includes('/fragment/') ? 'fragment' : 'view';
    return {
      file: rel,
      type,
      lines: content.split('\n').length,
      topControls: collectControls(content)
    };
  })
};

const outPath = path.join(root, 'docs', 'ux', 'ui-entity-inventory.json');
fs.writeFileSync(outPath, JSON.stringify(report, null, 2));
console.log(`UI entity inventory generated: ${path.relative(root, outPath)} (${report.fileCount} files)`);
