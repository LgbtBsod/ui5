#!/usr/bin/env node
const fs = require('fs');

const path = 'docs/artifacts/theme-parity-report.json';
if (!fs.existsSync(path)) {
  console.error('Theme parity gate failed: report missing.');
  process.exit(1);
}

const report = JSON.parse(fs.readFileSync(path, 'utf8'));
const flows = report.touchedFlows || {};
const required = ['search', 'detail'];
for (const f of required) {
  if (!flows[f]) {
    console.error(`Theme parity gate failed: missing flow ${f}`);
    process.exit(1);
  }
  const entry = flows[f];
  if (entry.morning !== 'pass' || entry.night !== 'pass' || entry.parity !== 'pass') {
    console.error(`Theme parity gate failed: ${f} is not pass in all dimensions`);
    process.exit(1);
  }
}

if (!report.summary || report.summary.parityPassRate < 1 || report.summary.criticalDiffs !== 0) {
  console.error('Theme parity gate failed: summary thresholds not met.');
  process.exit(1);
}

console.log('Theme parity gate passed: Morning/Night parity report is fully green.');
