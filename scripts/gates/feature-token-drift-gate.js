#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { detectRuntimeRoot, readText } = require('../qa-shared');
const { exitWithMappedIssues } = require('../lib/gate-result');

const ROOT = process.cwd();
const RUNTIME_ROOT = detectRuntimeRoot(ROOT);

const SCAN_DIRS = [
  path.join(RUNTIME_ROOT, 'service', 'features'),
  path.join(RUNTIME_ROOT, 'controller'),
  path.join(RUNTIME_ROOT, 'infra', 'navigation')
];

const RULES = [
  {
    label: 'layout-token',
    token: 'OneColumn',
    allowedFiles: new Set([
      `${RUNTIME_ROOT}/contracts/NavigationContracts.js`,
      `${RUNTIME_ROOT}/service/contracts/NavigationContracts.js`,
      `${RUNTIME_ROOT}/model/schema/navigationSchema.js`,
      `${RUNTIME_ROOT}/manifest.json`
    ])
  },
  {
    label: 'search-load-error',
    token: 'Search request failed',
    allowedFiles: new Set([
      `${RUNTIME_ROOT}/service/contracts/ProgressiveReadinessContracts.js`
    ])
  },
  {
    label: 'startup-event',
    token: 'firstRouteReady',
    allowedFiles: new Set([
      `${RUNTIME_ROOT}/service/contracts/ProgressiveReadinessContracts.js`,
      `${RUNTIME_ROOT}/Component.js`
    ])
  },
  {
    label: 'startup-event',
    token: 'analyticsStarted',
    allowedFiles: new Set([
      `${RUNTIME_ROOT}/service/contracts/ProgressiveReadinessContracts.js`,
      `${RUNTIME_ROOT}/Component.js`
    ])
  }
];

function escapeRegExp(value) {
  return String(value || '').replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function collectFiles() {
  const files = [];
  SCAN_DIRS.forEach((dir) => {
    if (!fs.existsSync(path.join(ROOT, dir))) {
      return;
    }
    const walk = (absDir) => {
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
    walk(path.join(ROOT, dir));
  });
  return files.sort();
}

function findLine(source, token) {
  const index = source.search(new RegExp(`["']${escapeRegExp(token)}["']`));
  return index < 0 ? null : source.slice(0, index).split(/\r?\n/).length;
}

function main() {
  const issues = [];
  collectFiles().forEach((absPath) => {
    const relPath = path.relative(ROOT, absPath).replace(/\\/g, '/');
    const source = readText(ROOT, relPath);
    RULES.forEach((rule) => {
      if (rule.allowedFiles.has(relPath)) {
        return;
      }
      const pattern = new RegExp(`["']${escapeRegExp(rule.token)}["']`);
      if (pattern.test(source)) {
        issues.push({
          file: relPath,
          line: findLine(source, rule.token),
          message: `${rule.label} must come from canonical contract, found literal "${rule.token}"`
        });
      }
    });
  });

  exitWithMappedIssues('feature-token-drift-gate', issues, (item) => item, { rules: RULES.length }, { asJson: process.argv.includes('--json') });
}

main();
