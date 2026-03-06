#!/usr/bin/env node

const path = require('path');
const { listFiles } = require('./lib/fileWalker');
const { readSafe } = require('./lib/textRead');
const { sha256 } = require('./lib/hashUtils');
const { createGateResult, finalizeAndExit } = require('./lib/gate-result');

const root = path.resolve(__dirname, '..');
const files = listFiles(root, { include: ['service/backend/**/*.js'] });
const hashes = new Map();

files.forEach((file) => {
  const read = readSafe(root, file);
  if (!read.ok) return;
  const key = sha256(read.text.replace(/\s+/g, ' ').trim());
  hashes.set(key, [...(hashes.get(key) || []), file]);
});

const duplicates = [...hashes.values()].filter((group) => group.length > 1);
const result = createGateResult(
  'backend-service-dedup-gate',
  duplicates.map((group) => ({
    file: group[0],
    message: `duplicate backend service implementation: ${group.join(', ')}`
  })),
  { filesScanned: files.length }
);
finalizeAndExit(result, { asJson: process.argv.includes('--json') });
