#!/usr/bin/env node
const { failWith, requireJsonReport } = require('./lib/reportGateRuntime');

const path = 'docs/artifacts/theme-parity-report.json';
const report = requireJsonReport(path, {
  prefix: 'Theme parity gate failed',
  missingExitCode: 1
});
const flows = report.touchedFlows || {};
const required = ['search', 'detail'];
for (const f of required) {
  if (!flows[f]) {
    failWith('Theme parity gate failed', `missing flow ${f}`, 1);
  }
  const entry = flows[f];
  if (entry.morning !== 'pass' || entry.night !== 'pass' || entry.parity !== 'pass') {
    failWith('Theme parity gate failed', `${f} is not pass in all dimensions`, 1);
  }
}

if (!report.summary || report.summary.parityPassRate < 1 || report.summary.criticalDiffs !== 0) {
  failWith('Theme parity gate failed', 'summary thresholds not met.', 1);
}

console.log('Theme parity gate passed: Morning/Night parity report is fully green.');
