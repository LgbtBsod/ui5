#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

const root = process.cwd();
const offenders = [];

function walk(dir) {
  for (const e of fs.readdirSync(dir, { withFileTypes: true })) {
    if (e.name === '.git' || e.name === 'node_modules') continue;
    const p = path.join(dir, e.name);
    if (e.isDirectory()) walk(p);
    else if (e.isFile() && p.endsWith('.js')) {
      const rel = path.relative(root, p).replace(/\\/g, '/');
      const txt = fs.readFileSync(p, 'utf8');
      if (/setProperty\(\s*["']\/sessionId["']/.test(txt) && rel !== 'Component.js') offenders.push(rel);
    }
  }
}

walk(root);
if (offenders.length) {
  console.error('sessionid-writes-gate failed. Offenders:\n' + offenders.join('\n'));
  process.exit(1);
}
console.log('sessionid-writes-gate passed');
