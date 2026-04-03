#!/usr/bin/env node

const { requireJsonReport } = require('./lib/reportGateRuntime');
const { exitWithColonIssues } = require('./lib/issueGateRuntime');

const reportPath = process.argv[2] || 'docs/artifacts/gateway-only-smoke-report.json';

const report = requireJsonReport(reportPath, {
  prefix: 'Gateway smoke gate failed',
  missingExitCode: 2
});
const apiFailures = (((report || {}).api || {}).failures) || [];
const browserFailures = (((report || {}).browser || {}).failures) || [];
const issues = [];

if (report.status !== 'ok') {
  issues.push(`${reportPath}: report status must be "ok"`);
}
apiFailures.forEach((name) => issues.push(`${reportPath}: api failure ${name}`));
browserFailures.forEach((name) => issues.push(`${reportPath}: browser failure ${name}`));

exitWithColonIssues(
  'gateway-only-smoke-gate',
  issues,
  {
  createdDbKey: report.createdDbKey || '-',
    apiChecks: (((report || {}).api || {}).checks || []).length,
    browserChecks: (((report || {}).browser || {}).checks || []).length
  },
  { asJson: process.argv.includes('--json') }
);
