#!/usr/bin/env node
const fs = require('fs');

const catalogPath = 'docs/artifacts/message-taxonomy-meta.json';
if (!fs.existsSync(catalogPath)) {
  console.error('C4 gate failed: docs/artifacts/message-taxonomy-meta.json is missing.');
  process.exit(1);
}
const meta = JSON.parse(fs.readFileSync(catalogPath, 'utf8'));
const allowed = new Set(['info', 'success', 'warning', 'error', 'critical']);
const keys = Object.keys(meta || {});
if (!keys.length) {
  console.error('C4 gate failed: empty taxonomy catalog.');
  process.exit(1);
}
for (const key of keys) {
  const item = meta[key];
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
console.log(`C4 gate passed: ${keys.length} taxonomy entries validated.`);
