#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { detectRuntimeRoot, readText } = require('../qa-shared');
const { exitWithMappedIssues } = require('../lib/gate-result');

const ROOT = process.cwd();
const RUNTIME_ROOT = detectRuntimeRoot(ROOT);
const SCAN_DIRS = [
  path.join(ROOT, RUNTIME_ROOT, 'service', 'features'),
  path.join(ROOT, RUNTIME_ROOT, 'service', 'domain', 'shared'),
  path.join(ROOT, RUNTIME_ROOT, 'util'),
  path.join(ROOT, RUNTIME_ROOT, 'infra', 'adapters')
];

const SIGNATURES = [
  { key: 'normalizeChecklistIds', minHits: 2 },
  { key: 'extractChecklistId', minHits: 2 },
  { key: 'formatHumanDateTime', minHits: 2 },
  { key: 'buildSaveBannerPayload', minHits: 2 },
  { key: 'loadDetailSnapshot', minHits: 2 }
];

function hasLocalDefinition(source, key) {
  const patterns = [
    new RegExp(`function\\s+${key}\\s*\\(`),
    new RegExp(`\\b${key}\\s*:\\s*function\\s*\\(`),
    new RegExp(`\\bconst\\s+${key}\\s*=\\s*function\\s*\\(`),
    new RegExp(`\\bvar\\s+${key}\\s*=\\s*function\\s*\\(`)
  ];
  return patterns.some((pattern) => pattern.test(source));
}

function collectFiles() {
  const files = [];
  const walk = (absDir) => {
    if (!fs.existsSync(absDir)) {
      return;
    }
    fs.readdirSync(absDir, { withFileTypes: true }).forEach((entry) => {
      const absPath = path.join(absDir, entry.name);
      if (entry.isDirectory()) {
        walk(absPath);
        return;
      }
      if (entry.name.endsWith('.js')) {
        files.push(absPath);
      }
    });
  };
  SCAN_DIRS.forEach(walk);
  return files.sort();
}

function main() {
  const files = collectFiles();
  const issues = [];
  SIGNATURES.forEach((signature) => {
    const hits = files.filter((absPath) => {
      const relPath = path.relative(ROOT, absPath).replace(/\\/g, '/');
      return hasLocalDefinition(readText(ROOT, relPath), signature.key);
    });
    if (hits.length >= signature.minHits) {
      issues.push({
        file: path.relative(ROOT, hits[0]).replace(/\\/g, '/'),
        message: `duplicate responsibility signature "${signature.key}" appears in ${hits.length} files`
      });
    }
  });
  exitWithMappedIssues('duplicate-responsibility-gate', issues, (item) => item, { scannedFiles: files.length }, { asJson: process.argv.includes('--json'), advisory: true });
}

main();
