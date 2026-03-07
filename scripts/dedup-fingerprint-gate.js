#!/usr/bin/env node

const path = require('path');
const { listFiles } = require('./lib/fileWalker');
const { readSafe } = require('./lib/textRead');
const { sha256 } = require('./lib/hashUtils');
const { extractFunctions } = require('./lib/functionExtract');
const { fingerprint } = require('./lib/fingerprint');

const root = path.resolve(__dirname, '..');
const files = listFiles(root, { include: ['controller/**/*.js', 'service/**/*.js', 'infra/**/*.js', 'ports/**/*.js', 'model/**/*.js', 'util/**/*.js'] });
const fileHashes = new Map();
const fnHashes = new Map();

function isTinyWrapper(fn) {
  return fn.length <= 15;
}

files.forEach((file) => {
  const read = readSafe(root, file);
  if (!read.ok) return;
  const compact = read.text.replace(/\s+/g, ' ').trim();
  fileHashes.set(sha256(compact), [...(fileHashes.get(sha256(compact)) || []), file]);
  extractFunctions(read.text)
    .filter((fn) => !isTinyWrapper(fn))
    .forEach((fn) => {
      const key = fingerprint(fn.text);
      const arr = fnHashes.get(key) || [];
      if (!arr.includes(file)) arr.push(file);
      fnHashes.set(key, arr);
    });
});

const dupFiles = [...fileHashes.values()].filter((group) => group.length > 1);
const dupFns = [...fnHashes.values()].filter((group) => group.length > 1);
const failFns = dupFns.filter((group) => group.length >= 3);
const warnFns = dupFns.filter((group) => group.length === 2);

if (warnFns.length) {
  console.warn('WARN dedup-fingerprint-gate');
  warnFns.slice(0, 20).forEach((group) => console.warn(`- duplicate function in 2 files: ${group.join(', ')}; consider scripts/lib extraction`));
}
if (dupFiles.length || failFns.length) {
  console.error('FAIL dedup-fingerprint-gate');
  dupFiles.forEach((group) => console.error(`- identical file sha256 duplicate: ${group.join(', ')}`));
  failFns.forEach((group) => console.error(`- duplicate function fingerprint in >=3 files: ${group.join(', ')}`));
  process.exit(1);
}
console.log('PASS dedup-fingerprint-gate');
