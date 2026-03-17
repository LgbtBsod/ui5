#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { detectRuntimeRoot, readText } = require('../qa-shared');
const { exitWithMappedIssues } = require('../lib/gate-result');

const ROOT = process.cwd();
const RUNTIME_ROOT = detectRuntimeRoot(ROOT);
const FRAMEWORK_DIR = path.join(ROOT, RUNTIME_ROOT, 'service', 'framework');

function collectFrameworkFiles() {
  if (!fs.existsSync(FRAMEWORK_DIR)) {
    return [];
  }
  return fs.readdirSync(FRAMEWORK_DIR)
    .filter((name) => name.endsWith('.js'))
    .map((name) => path.join(FRAMEWORK_DIR, name))
    .sort();
}

function countFunctionDeclarations(source) {
  return (source.match(/\bfunction\b/g) || []).length;
}

function parseDependencies(source) {
  const match = source.match(/sap\.ui\.define\s*\(\s*\[([\s\S]*?)\]\s*,\s*function\s*\(([\s\S]*?)\)\s*\{/);
  if (!match) {
    return [];
  }
  const depNames = match[2]
    .split(',')
    .map((item) => String(item || '').trim())
    .filter(Boolean);
  return depNames;
}

function parseReturnedProperties(source) {
  const match = source.match(/return\s*\{([\s\S]*?)\};/);
  if (!match) {
    return null;
  }
  return match[1]
    .split(',')
    .map((line) => line.trim())
    .filter(Boolean);
}

function isSimpleAliasValue(value, dependencies) {
  const normalized = String(value || '').trim();
  if (!normalized) {
    return false;
  }
  if (!/^[A-Za-z_$][A-Za-z0-9_$]*(\.[A-Za-z_$][A-Za-z0-9_$]*)+$/.test(normalized)) {
    return false;
  }
  const rootName = normalized.split('.')[0];
  return dependencies.includes(rootName);
}

function detectAliasOnlyFile(absPath) {
  const relPath = path.relative(ROOT, absPath).replace(/\\/g, '/');
  const source = readText(ROOT, relPath);
  const dependencies = parseDependencies(source);
  const returnProps = parseReturnedProperties(source);
  const functionCount = countFunctionDeclarations(source);

  if (dependencies.length === 0 || dependencies.length > 3 || !returnProps || functionCount > 1) {
    return null;
  }

  const aliasProps = returnProps.filter((entry) => {
    const parts = entry.split(':');
    if (parts.length !== 2) {
      return false;
    }
    return isSimpleAliasValue(parts[1], dependencies);
  });

  if (aliasProps.length === 0 || aliasProps.length !== returnProps.length) {
    return null;
  }

  return {
    file: relPath,
    message: 'alias-only framework file detected; import canonical runtime directly'
  };
}

function main() {
  const files = collectFrameworkFiles();
  const issues = files
    .map(detectAliasOnlyFile)
    .filter(Boolean);

  exitWithMappedIssues(
    'framework-alias-gate',
    issues,
    (item) => item,
    { scannedFiles: files.length },
    { asJson: process.argv.includes('--json') }
  );
}

main();
