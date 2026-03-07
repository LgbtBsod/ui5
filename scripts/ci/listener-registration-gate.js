#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { collectFilesByExtensions, normalizePath } = require('../qa-shared');
const { createGateResult, finalizeAndExit } = require('../lib/gate-result');

const root = path.resolve(__dirname, '..', '..');
const files = collectFilesByExtensions(root, ['.', 'controller', 'service', 'util'], ['.js'])
  .filter((file) => !file.startsWith('scripts/'))
  .filter((file) => !file.startsWith('backend/mock_gateway/'))
  .filter((file) => !file.startsWith('mock_gate_way/'))
  .filter((file) => !file.startsWith('node_modules/'));

const issues = [];

function countMatches(text, regex) {
  const matches = text.match(regex);
  return matches ? matches.length : 0;
}

function checkDuplicateRouteHooks(file, text) {
  const routeHooks = new Map();
  const routeRegex = /attachRouteMatched\s*\(\s*["']([^"']+)["']\s*,\s*([A-Za-z0-9_$.]+)/g;
  let match;
  while ((match = routeRegex.exec(text))) {
    const key = `${match[1]}::${match[2]}`;
    routeHooks.set(key, (routeHooks.get(key) || 0) + 1);
  }
  routeHooks.forEach((count, key) => {
    if (count > 1) {
      issues.push(`${file}: duplicate attachRouteMatched registration for ${key} (${count} times)`);
    }
  });
}

function checkDuplicatePropertyHooks(file, text) {
  const propHooks = new Map();
  const propRegex = /([A-Za-z0-9_$]+)\.attachPropertyChange\s*\(\s*([A-Za-z0-9_$.]+)\s*(?:,|\))/g;
  let match;
  while ((match = propRegex.exec(text))) {
    const key = `${match[1]}::${match[2]}`;
    propHooks.set(key, (propHooks.get(key) || 0) + 1);
  }
  propHooks.forEach((count, key) => {
    if (count > 1) {
      issues.push(`${file}: duplicate attachPropertyChange registration for ${key} (${count} times)`);
    }
  });
}

function checkRouteHookTeardown(file, text) {
  const routeAttachCount = countMatches(text, /attachRouteMatched\s*\(/g) + countMatches(text, /attachPatternMatched\s*\(/g);
  const routeDetachCount = countMatches(text, /detachRouteMatched\s*\(/g) + countMatches(text, /detachAllRouteMatched\s*\(/g) + countMatches(text, /detachPatternMatched\s*\(/g);
  if (routeAttachCount > 0 && routeDetachCount === 0) {
    issues.push(`${file}: route listener registration has no paired detach`);
  }
}

function checkPropertyHookTeardown(file, text) {
  const propAttachCount = countMatches(text, /\.attachPropertyChange\s*\(/g);
  const propDetachCount = countMatches(text, /\.detachPropertyChange\s*\(/g);
  if (propAttachCount > 0 && propDetachCount === 0) {
    issues.push(`${file}: propertyChange listener registration has no paired detach`);
  }
}

files.forEach((file) => {
  const abs = path.join(root, file);
  const text = fs.readFileSync(abs, 'utf8');

  if (countMatches(text, /\bgetRouter\s*\(\)\.initialize\s*\(/g) + countMatches(text, /\boRouter\.initialize\s*\(/g) > 1) {
    issues.push(`${file}: router.initialize() appears more than once`);
  }

  const beforeUnloadAdds = countMatches(text, /addEventListener\s*\(\s*["']beforeunload["']/g);
  const beforeUnloadRemoves = countMatches(text, /removeEventListener\s*\(\s*["']beforeunload["']/g);
  if (beforeUnloadAdds > 1) {
    issues.push(`${file}: beforeunload listener registered more than once (${beforeUnloadAdds})`);
  }
  if (beforeUnloadAdds > 0 && beforeUnloadRemoves === 0) {
    issues.push(`${file}: beforeunload listener has no paired removeEventListener`);
  }

  if (countMatches(text, /attachBeforeRouteMatched\s*\(/g) > 1) {
    issues.push(`${file}: attachBeforeRouteMatched registered more than once`);
  }

  checkRouteHookTeardown(file, text);
  checkDuplicateRouteHooks(file, text);
  checkPropertyHookTeardown(file, text);
  checkDuplicatePropertyHooks(file, text);
});

const result = createGateResult(
  'listener-registration-gate',
  issues.map((issue) => {
    const normalized = normalizePath(issue);
    return { file: String(normalized).split(':')[0], message: normalized };
  }),
  { filesScanned: files.length }
);
finalizeAndExit(result, { asJson: process.argv.includes('--json') });
