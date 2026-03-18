#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { listFiles } = require('./lib/fileWalker');
const { readJsonSafe } = require('./lib/auditInput');
const { readSafe } = require('./lib/textRead');
const { extractFunctions } = require('./lib/functionExtract');
const { fingerprint } = require('./lib/fingerprint');

const ROOT = path.resolve(__dirname, '..');
const ALLOWLIST_PATH = path.join(ROOT, 'scripts', 'runtime-duplicate-allowlist.json');
const INCLUDE = ['controller/**/*.js', 'service/**/*.js', 'infra/**/*.js', 'ports/**/*.js', 'model/**/*.js', 'util/**/*.js'];

function readAllowlist() {
  if (!fs.existsSync(ALLOWLIST_PATH)) {
    return new Set();
  }
  return new Set(readJsonSafe(ALLOWLIST_PATH, []));
}

function makeClusterKey(files) {
  return [...files].sort().join(' | ');
}

function main() {
  const allowlist = readAllowlist();
  const files = listFiles(ROOT, { include: INCLUDE });
  const fpToEntries = new Map();
  const violations = [];

  files.forEach((file) => {
    const read = readSafe(ROOT, file);
    if (!read.ok) {
      return;
    }
    extractFunctions(read.text)
      .filter((fn) => fn.length >= 8 && fn.text.length >= 180)
      .forEach((fn) => {
        const fp = fingerprint(fn.text);
        const entries = fpToEntries.get(fp) || [];
        entries.push({ file, name: fn.name, line: fn.startLine });
        fpToEntries.set(fp, entries);
      });
  });

  [...fpToEntries.values()].forEach((entries) => {
    const uniqueFiles = [...new Set(entries.map((entry) => entry.file))];
    if (uniqueFiles.length < 2) {
      return;
    }
    if (allowlist.has(makeClusterKey(uniqueFiles))) {
      return;
    }
    violations.push({
      files: uniqueFiles,
      sample: entries.slice(0, 4).map((entry) => `${entry.file}:${entry.line}#${entry.name}`).join(', ')
    });
  });

  if (violations.length) {
    const advisory = process.env.QA_STRICT_STRUCTURAL !== '1';
    console.log(`${advisory ? 'WARN' : 'FAIL'} runtime-duplication-gate`);
    violations.slice(0, 30).forEach((item) => {
      console.log(`- duplicate runtime logic cluster: ${item.files.join(', ')}`);
      console.log(`  sample: ${item.sample}`);
    });
    process.exit(advisory ? 0 : 1);
  }

  console.log('PASS runtime-duplication-gate');
}

main();
