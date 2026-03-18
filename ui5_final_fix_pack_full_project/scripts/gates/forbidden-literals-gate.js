#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { detectRuntimeRoot, readText } = require('../qa-shared');
const { exitWithMappedIssues } = require('../lib/gate-result');

const ROOT = process.cwd();
const RUNTIME_ROOT = detectRuntimeRoot(ROOT);
const APP_ROOT = path.join(ROOT, RUNTIME_ROOT);

const RULES = [
  {
    label: 'layout-literal',
    token: 'OneColumn',
    allowed: new Set([
      `${RUNTIME_ROOT}/contracts/NavigationContracts.js`,
      `${RUNTIME_ROOT}/service/contracts/NavigationContracts.js`,
      `${RUNTIME_ROOT}/model/schema/navigationSchema.js`,
      `${RUNTIME_ROOT}/manifest.json`
    ])
  },
  {
    label: 'listener-event',
    token: 'pcct:fullSave',
    allowed: new Set([
      `${RUNTIME_ROOT}/service/framework/ComponentListenerContracts.js`,
      `${RUNTIME_ROOT}/Component.js`
    ])
  }
];

function escapeRegExp(value) {
  return String(value || '').replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function collectFiles(dir, out) {
  fs.readdirSync(dir, { withFileTypes: true }).forEach((entry) => {
    if (entry.name === 'node_modules' || entry.name === '.git' || entry.name === 'docs') {
      return;
    }
    const absPath = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      collectFiles(absPath, out);
      return;
    }
    if (/\.(js|json)$/.test(entry.name)) {
      out.push(absPath);
    }
  });
}

function main() {
  const files = [];
  const issues = [];
  collectFiles(APP_ROOT, files);
  files.sort().forEach((absPath) => {
    const relPath = path.relative(ROOT, absPath).replace(/\\/g, '/');
    const source = readText(ROOT, relPath);
    RULES.forEach((rule) => {
      if (rule.allowed.has(relPath)) {
        return;
      }
      const pattern = new RegExp(`["']${escapeRegExp(rule.token)}["']`);
      if (pattern.test(source)) {
        issues.push({
          file: relPath,
          message: `${rule.label} must be referenced through canonical contract, found literal "${rule.token}"`
        });
      }
    });
  });
  exitWithMappedIssues('forbidden-literals-gate', issues, (item) => item, { scannedFiles: files.length }, { asJson: process.argv.includes('--json') });
}

main();
