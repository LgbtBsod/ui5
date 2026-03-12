#!/usr/bin/env node
const fs = require('fs');
const { resolveFromRoot } = require('../qa-shared');

const rootDir = process.cwd();
const statePathsSrc = fs.readFileSync(resolveFromRoot(rootDir, 'model/StatePaths.js'), 'utf8');
const schemaText = [
  fs.readFileSync(resolveFromRoot(rootDir, 'model/schema/uiSchema.js'), 'utf8'),
  fs.readFileSync(resolveFromRoot(rootDir, 'model/schema/workflowSchema.js'), 'utf8'),
  fs.readFileSync(resolveFromRoot(rootDir, 'model/schema/navigationSchema.js'), 'utf8')
].join('\n');

const paths = [...statePathsSrc.matchAll(/:\s*["']([^"']+)["']/g)].map((m) => m[1]);
const runtimeOnly = new Set(['/inlineErrors', '/conflictDialog', '/ui/feedback/conflictDialog']);

function pathLooksRepresented(pathValue) {
  if (!pathValue.startsWith('/') || runtimeOnly.has(pathValue)) {
    return true;
  }
  const parts = pathValue.split('/').filter(Boolean);
  return parts.every((part) => new RegExp(`\\b${part}\\s*:`).test(schemaText));
}

const bad = paths.filter((p) => !pathLooksRepresented(p));
if (bad.length) {
  console.error('FAIL missing paths:\n' + bad.join('\n'));
  process.exit(1);
}
console.log('PASS statepaths-schema-consistency-gate');
