#!/usr/bin/env node
const fs = require('fs');

const catalogPath = 'docs/ux/message-catalog-meta.json';
if (!fs.existsSync(catalogPath)) {
  console.error('C4 gate failed: docs/ux/message-catalog-meta.json is missing.');
  process.exit(1);
}
const meta = JSON.parse(fs.readFileSync(catalogPath, 'utf8'));
const allowed = new Set(['info', 'success', 'warning', 'error', 'critical']);
const i18nKeys = fs.readFileSync('i18n/i18n.properties', 'utf8')
  .split(/\r?\n/)
  .map((l) => l.trim())
  .filter((l) => l && !l.startsWith('#') && l.includes('='))
  .map((l) => l.split('=')[0].trim());

for (const key of i18nKeys) {
  const item = meta[key];
  if (!item) {
    console.error(`C4 gate failed: missing metadata for key ${key}`);
    process.exit(1);
  }
  if (!allowed.has(item.severity)) {
    console.error(`C4 gate failed: invalid severity for key ${key}`);
    process.exit(1);
  }
  if (!item.owner || !item.tone) {
    console.error(`C4 gate failed: owner/tone missing for key ${key}`);
    process.exit(1);
  }
  if (!/^[a-z][a-zA-Z0-9]*$/.test(key)) {
    console.error(`C4 gate failed: key naming violation ${key}`);
    process.exit(1);
  }
}
console.log(`C4 gate passed: ${i18nKeys.length} i18n keys have taxonomy metadata.`);
