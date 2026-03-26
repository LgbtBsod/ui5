#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { collectFilesByExtensions, extractUi5Dependencies, readText, lineFromIndex, detectRuntimeRoot } = require('./qa-shared');

const ROOT = process.cwd();
const RUNTIME_ROOT = detectRuntimeRoot(ROOT);
const JS_FILES = collectFilesByExtensions(ROOT, [RUNTIME_ROOT, 'scripts'], ['.js']);
const issues = [];
const deletedLegacyModules = new Set([
  'PRODUCTION_CONTROL_CHECKLIST/controller/detail/internal/DetailViewBehavior',
  'PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime',
]);
const ABSOLUTE_MODULE_ALLOWLIST_PREFIXES = [
  'sap/',
  'jquery.sap.',
  'jquery.sap',
  'sap-ui',
  'PRODUCTION_CONTROL_CHECKLIST/test/'
];

function moduleToRelativeFile(moduleName) {
  const normalized = String(moduleName || '').trim();
  const prefix = 'PRODUCTION_CONTROL_CHECKLIST/';
  if (!normalized.startsWith(prefix)) {
    return null;
  }
  return path.join(RUNTIME_ROOT, normalized.slice(prefix.length) + '.js');
}

function isAllowlistedAbsoluteModule(moduleName) {
  return ABSOLUTE_MODULE_ALLOWLIST_PREFIXES.some((prefix) => String(moduleName || '').startsWith(prefix));
}

function resolveRelativeModule(file, moduleName) {
  const normalized = String(moduleName || '').trim();
  if (!normalized || normalized.indexOf('.') !== 0) {
    return null;
  }
  return path.resolve(path.dirname(path.join(ROOT, file)), normalized + '.js');
}

JS_FILES.forEach((file) => {
  const source = readText(ROOT, file);
  extractUi5Dependencies(source).forEach(({ dep, index }) => {
    if (deletedLegacyModules.has(dep)) {
      issues.push(`${file}:${lineFromIndex(source, index)} deleted legacy module ${dep}`);
      return;
    }
    const relFile = moduleToRelativeFile(dep);
    const relativeFile = resolveRelativeModule(file, dep);
    if (!relFile && !relativeFile && !isAllowlistedAbsoluteModule(dep)) {
      issues.push(`${file}:${lineFromIndex(source, index)} unresolved application module ${dep}`);
      return;
    }
    if (!relFile && !relativeFile) {
      return;
    }
    const absFile = relFile ? path.join(ROOT, relFile) : relativeFile;
    if (!fs.existsSync(absFile)) {
      issues.push(`${file}:${lineFromIndex(source, index)} missing module ${dep}`);
    }
  });
});

if (issues.length) {
  console.log(['FAIL broken-import-gate', ...issues.map((issue) => `- ${issue}`)].join('\n'));
  process.exit(1);
}

console.log('PASS broken-import-gate');
