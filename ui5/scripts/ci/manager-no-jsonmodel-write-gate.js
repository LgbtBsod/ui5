#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { createGateResult, finalizeAndExit } = require('../lib/gate-result');

const root = path.resolve(__dirname, '../..');
const managerDir = path.join(root, 'manager');
const offenders = [];

if (fs.existsSync(managerDir)) {
  for (const name of fs.readdirSync(managerDir)) {
    if (!name.endsWith('.js')) continue;
    const full = path.join(managerDir, name);
    const text = fs.readFileSync(full, 'utf8');
    if (text.includes('.setProperty(')) offenders.push(`manager/${name}`);
  }
}

const result = createGateResult(
  'manager-no-jsonmodel-write-gate',
  offenders.map((file) => ({ file, message: 'manager model mutation via setProperty detected' })),
  { filesScanned: fs.existsSync(managerDir) ? fs.readdirSync(managerDir).filter((name) => name.endsWith('.js')).length : 0 }
);
finalizeAndExit(result, { asJson: process.argv.includes('--json') });
