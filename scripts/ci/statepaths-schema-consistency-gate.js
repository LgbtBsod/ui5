#!/usr/bin/env node
const fs = require('fs');
const statePathsSrc = fs.readFileSync('service/domain/shared/StatePaths.js', 'utf8');
const keys = [...statePathsSrc.matchAll(/:\s*["']([^"']+)["']/g)].map((m) => m[1]);
const root = {
  ...eval('(' + fs.readFileSync('model/schema/uiSchema.js','utf8').match(/return\s+(\{[\s\S]*?\});/)[1] + ')'),
  ...eval('(' + fs.readFileSync('model/schema/workflowSchema.js','utf8').match(/return\s+(\{[\s\S]*?\});/)[1] + ')'),
  ...eval('(' + fs.readFileSync('model/schema/navigationSchema.js','utf8').match(/return\s+(\{[\s\S]*?\});/)[1] + ')')
};
const runtimeOnly = new Set(['/inlineErrors','/conflictDialog']);
function existsPath(path) {
  if (runtimeOnly.has(path)) return true;
  const parts = path.split('/').filter(Boolean);
  let cur = root;
  for (const p of parts) {
    if (!cur || !(p in cur)) return false;
    cur = cur[p];
  }
  return true;
}
const bad = keys.filter((p) => p.startsWith('/') && !existsPath(p));
if (bad.length) { console.error('FAIL missing paths:\n' + bad.join('\n')); process.exit(1); }
console.log('PASS statepaths-schema-consistency-gate');
