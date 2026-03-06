#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { createGateResult, finalizeAndExit } = require('../lib/gate-result');

const root = path.resolve(__dirname, '../..');
const dir = path.join(root, 'controller');
const bad = [];

for (const name of fs.readdirSync(dir)) {
  if (!name.endsWith('.js')) continue;
  const full = path.join(dir, name);
  const text = fs.readFileSync(full, 'utf8');
  const imports = [...text.matchAll(/['"]([^'"]+)['"]/g)].map((m) => m[1]);
  imports.forEach((imp) => {
    if (/infra\/|service\/backend|GatewayODataClient|BackendAdapter/.test(imp)) {
      bad.push(`${name} -> ${imp}`);
    }
  });
}

const result = createGateResult(
  'controller-only-facade-gate',
  bad.map((entry) => ({ file: `controller/${entry.split(' -> ')[0]}`, message: entry })),
  { filesScanned: fs.readdirSync(dir).filter((name) => name.endsWith('.js')).length }
);
finalizeAndExit(result, { asJson: process.argv.includes('--json') });
