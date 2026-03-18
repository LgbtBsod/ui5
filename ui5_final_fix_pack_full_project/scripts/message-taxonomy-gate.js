#!/usr/bin/env node
const { requireJsonReport, failWith } = require('./lib/reportGateRuntime');

const catalogPath = 'docs/artifacts/message-taxonomy-meta.json';
const meta = requireJsonReport(catalogPath, { prefix: 'C4 gate failed', missingExitCode: 1, invalidExitCode: 1 });
const allowed = new Set(['info', 'success', 'warning', 'error', 'critical']);
const keys = Object.keys(meta || {});
if (!keys.length) {
  failWith('C4 gate failed', 'empty taxonomy catalog.', 1);
}
for (const key of keys) {
  const item = meta[key];
  if (!allowed.has(item.severity)) {
    failWith('C4 gate failed', `invalid severity for key ${key}`, 1);
  }
  if (!item.owner || !item.tone) {
    failWith('C4 gate failed', `owner/tone missing for key ${key}`, 1);
  }
  if (!/^[a-z][a-zA-Z0-9]*$/.test(key)) {
    failWith('C4 gate failed', `key naming violation ${key}`, 1);
  }
}
console.log(`C4 gate passed: ${keys.length} taxonomy entries validated.`);
