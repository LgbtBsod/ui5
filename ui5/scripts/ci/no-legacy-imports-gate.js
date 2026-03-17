#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

const root = path.resolve(__dirname, '../..');
const hits = [];

function walk(dir) {
  for (const name of fs.readdirSync(dir)) {
    if (['.git', 'node_modules'].includes(name)) continue;
    const full = path.join(dir, name);
    const stat = fs.statSync(full);
    if (stat.isDirectory()) walk(full);
    else if (/\.js$/.test(name)) {
      const rel = path.relative(root, full).replace(/\\/g, '/');
      if (!/^(controller|service|facades|manager|managers|infra|ports)\//.test(rel)) continue;
      const text = fs.readFileSync(full, 'utf8');
      if (/service\/usecase|\/legacy_quarantine/.test(text)) hits.push(rel);
    }
  }
}

walk(root);
if (hits.length) {
  console.error('FAIL no-legacy-imports-gate');
  hits.slice(0, 50).forEach((h) => console.error('-', h));
  process.exit(1);
}
console.log('PASS no-legacy-imports-gate');
