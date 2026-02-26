#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();
const INCLUDE_DIRS = ['view', 'controller', 'service', 'util', '.'];

function walk(dir, out) {
  fs.readdirSync(dir, { withFileTypes: true }).forEach((e) => {
    if (e.name === '.git' || e.name === 'node_modules') return;
    const p = path.join(dir, e.name);
    if (e.isDirectory()) return walk(p, out);
    if (e.isFile()) out.push(p);
  });
}

function rel(p) { return path.relative(ROOT, p).replace(/\\/g, '/'); }

const files = [];
INCLUDE_DIRS.forEach((d) => {
  const p = path.join(ROOT, d);
  if (fs.existsSync(p)) walk(p, files);
});

const jsFiles = files.filter((f) => f.endsWith('.js'));
const xmlFiles = files.filter((f) => f.endsWith('.xml'));

const moduleUsage = new Map();
jsFiles.forEach((f) => {
  const text = fs.readFileSync(f, 'utf8');
  const modRe = /["'](sap\/[A-Za-z0-9_\/.-]+)["']/g;
  let m;
  while ((m = modRe.exec(text))) {
    const mod = m[1];
    if (!moduleUsage.has(mod)) moduleUsage.set(mod, new Set());
    moduleUsage.get(mod).add(rel(f));
  }
});

const controlUsage = new Map();
xmlFiles.forEach((f) => {
  const text = fs.readFileSync(f, 'utf8');
  const xmlnsMap = {};
  const xmlnsRe = /xmlns(?::([A-Za-z0-9_]+))?="([^"]+)"/g;
  let m;
  while ((m = xmlnsRe.exec(text))) {
    const prefix = m[1] || '';
    xmlnsMap[prefix] = m[2];
  }

  const tagRe = /<([A-Za-z0-9_]+:)?([A-Za-z0-9_]+)(\s|>|\/)/g;
  while ((m = tagRe.exec(text))) {
    const rawPrefix = (m[1] || '').replace(':', '');
    const tag = m[2];
    if (tag === 'FragmentDefinition' || tag === 'View') continue;
    const ns = xmlnsMap[rawPrefix] || '';
    if (!ns.startsWith('sap.')) continue;
    const full = `${ns}.${tag}`;
    if (!controlUsage.has(full)) controlUsage.set(full, new Set());
    controlUsage.get(full).add(rel(f));
  }
});

const toSorted = (map) => [...map.entries()]
  .map(([name, refs]) => ({ name, files: [...refs].sort() }))
  .sort((a, b) => a.name.localeCompare(b.name));

const report = {
  generatedAt: new Date().toISOString(),
  ui5VersionTarget: '1.71.28',
  modules: toSorted(moduleUsage),
  controls: toSorted(controlUsage)
};

const outJson = path.join(ROOT, 'docs', 'ux', 'ui5-api-usage.json');
fs.writeFileSync(outJson, JSON.stringify(report, null, 2));

const md = [];
md.push('# UI5 1.71.28 API usage list (project scan)');
md.push('');
md.push('Generated from source files. Contains all detected SAP modules and XML controls currently used in the codebase.');
md.push('');
md.push(`- Modules: ${report.modules.length}`);
md.push(`- Controls: ${report.controls.length}`);
md.push('');
md.push('## SAP modules');
report.modules.forEach((m) => {
  md.push(`- \`${m.name}\``);
});
md.push('');
md.push('## XML controls');
report.controls.forEach((c) => {
  md.push(`- \`${c.name}\``);
});

const outMd = path.join(ROOT, 'docs', 'ux', 'ui5-api-usage.md');
fs.writeFileSync(outMd, md.join('\n'));

console.log(`UI5 API usage report generated: ${rel(outJson)} and ${rel(outMd)}`);
