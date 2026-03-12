#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { exitWithMappedIssues } = require('../lib/gate-result');

const ROOT = process.cwd();
const ADAPTER_DIR = path.join(ROOT, 'app', 'infra', 'adapters');
const ALLOWED_FACTORY_FILES = new Set([
  'BrowserCacheAdapter.js',
  'DictAdapter.js',
  'ODataChecklistRepoAdapter.js',
  'SmartControlsAdapter.js',
  'TelemetryAdapter.js',
  'Ui5StateAdapter.js'
]);

function collectAdapterFiles() {
  if (!fs.existsSync(ADAPTER_DIR)) {
    return [];
  }
  return fs.readdirSync(ADAPTER_DIR)
    .filter((name) => name.endsWith('.js'))
    .map((name) => path.join(ADAPTER_DIR, name))
    .sort();
}

function read(absPath) {
  return fs.readFileSync(absPath, 'utf8');
}

function hasFactoryReturn(source) {
  return /return\s*\{\s*create\s*:\s*create\s*\}\s*;/.test(source);
}

function hasCreateFunction(source) {
  return /\bfunction\s+create\s*\(/.test(source);
}

function detectForbiddenFactory(absPath) {
  const fileName = path.basename(absPath);
  const source = read(absPath);
  const usesFactory = hasCreateFunction(source) || hasFactoryReturn(source);
  if (!usesFactory || ALLOWED_FACTORY_FILES.has(fileName)) {
    return null;
  }
  return {
    file: path.relative(ROOT, absPath).replace(/\\/g, '/'),
    message: 'adapter factory is forbidden here; use direct module exports unless the adapter captures injected runtime dependencies'
  };
}

function main() {
  const files = collectAdapterFiles();
  const issues = files.map(detectForbiddenFactory).filter(Boolean);
  exitWithMappedIssues(
    'adapter-factory-boundary-gate',
    issues,
    (item) => item,
    {
      allowedFactories: Array.from(ALLOWED_FACTORY_FILES).sort(),
      scannedFiles: files.length
    },
    { asJson: process.argv.includes('--json') }
  );
}

main();
