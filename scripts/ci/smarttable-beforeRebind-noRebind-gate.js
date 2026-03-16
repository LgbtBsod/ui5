#!/usr/bin/env node
const fs = require('fs');
const { resolveFromRoot } = require('../qa-shared');
const src = fs.readFileSync(resolveFromRoot(process.cwd(), 'controller/search/SearchSmartTableBehavior.js'), 'utf8');
const m = src.match(/function\s+onBeforeSmartTableRebind\s*\([^)]*\)\s*\{([\s\S]*?)\n\s*\}/);
if (!m) { console.error('FAIL: onBeforeSmartTableRebind not found'); process.exit(1); }
if (/rebindTable\s*\(|_execute\s*\(\s*["']rebind["']|\brebind\b/i.test(m[1])) {
  console.error('FAIL: beforeRebind must not trigger rebind');
  process.exit(1);
}
console.log('PASS smarttable-beforeRebind-noRebind-gate');
