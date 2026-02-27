#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

const targets = [
  'css/style.css',
  'css/modules/style-core.css',
  'css/modules/style-components.css',
  'css/modules/style-overrides.css'
].filter((p) => fs.existsSync(p));

function normalizeSelector(selector) {
  return selector.replace(/\s+/g, ' ').trim();
}

function findDuplicates(text) {
  const duplicates = [];
  const seen = new Map();
  const re = /(^|\n)\s*([^@\n][^{]+)\{/g;
  let m;
  while ((m = re.exec(text)) !== null) {
    const selectorRaw = m[2] || '';
    if (!selectorRaw.includes('.') && !selectorRaw.includes('#') && !selectorRaw.includes(':')) {
      continue;
    }
    const selector = normalizeSelector(selectorRaw);
    if (selector.startsWith('/*')) {
      continue;
    }
    if (seen.has(selector)) {
      duplicates.push(selector);
    } else {
      seen.set(selector, true);
    }
  }
  return [...new Set(duplicates)];
}

let hasDup = false;
for (const file of targets) {
  const text = fs.readFileSync(file, 'utf8');
  const dups = findDuplicates(text);
  if (dups.length) {
    hasDup = true;
    console.error(`Duplicate selector gate failed in ${file}:`);
    dups.slice(0, 20).forEach((d) => console.error(`- ${d}`));
  }
}

if (hasDup) {
  console.warn('Duplicate selector gate warning: duplicates detected (non-blocking in this phase).');
  console.warn('Planned tightening: switch to blocking mode after cleanup wave completes.');
  process.exit(0);
}

console.log('Duplicate selector gate passed: no duplicate selectors in style bundle.');
