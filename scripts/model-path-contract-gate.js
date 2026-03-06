#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { assert } = require('./gate-assert');

const root = path.resolve(__dirname, '..');
const requiredPaths = [
  '/ui/busy/global',
  '/ui/busy/searchTable',
  '/ui/busy/detail',
  '/workflow/detail/editMode',
  '/workflow/detail/lock/state',
  '/workflow/detail/autosave/state',
  '/workflow/detail/autosave/lastSavedAt',
  '/workflow/search/mode',
  '/workflow/search/segments'
];

function collectDomainFiles(dir) {
  return fs.readdirSync(dir).flatMap((name) => {
    const p = path.join(dir, name);
    const st = fs.statSync(p);
    if (st.isDirectory()) {
      return collectDomainFiles(p);
    }
    return p.endsWith('.js') ? [p] : [];
  });
}

(function main() {
  const files = collectDomainFiles(path.join(root, 'service/domain'));
  const text = files.map((f) => fs.readFileSync(f, 'utf8')).join('\n');

  requiredPaths.forEach((contractPath) => {
    assert(text.includes(contractPath), `Missing contract path usage: ${contractPath}`);
  });

  console.log('model-path-contract-gate PASS');
})();
