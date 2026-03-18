#!/usr/bin/env node

const { failWith, requireJsonReport } = require('./lib/reportGateRuntime');

const reportPath = process.argv[2] || 'docs/artifacts/gateway-only-smoke-report.json';
const REQUIRED_FLOW = 'attachmentDirtyInvariant';
const REQUIRED_CHECKS = [
  'detail.attachment_dirty.lock_acquired_clean',
  'detail.attachment_upload_keeps_clean_state',
  'detail.attachment_delete_keeps_clean_state',
  'detail.attachment_dirty.lock_release'
];

function fail(message) {
  failWith('Gateway smoke invariant gate failed', message, 1);
}

const report = requireJsonReport(reportPath, {
  prefix: 'Gateway smoke invariant gate failed',
  missingExitCode: 1
});
const browser = (report || {}).browser || {};
const flows = browser.flows || {};
const invariantFlow = flows[REQUIRED_FLOW] || null;
const flowChecks = (invariantFlow && invariantFlow.checks) || [];
const aggregateChecks = browser.checks || [];
const aggregateNames = new Set(aggregateChecks.map((item) => String((item || {}).name || '').trim()).filter(Boolean));
const flowNames = new Set(flowChecks.map((item) => String((item || {}).name || '').trim()).filter(Boolean));

if (!invariantFlow) {
  fail(`missing browser.flows.${REQUIRED_FLOW}`);
}

if (!invariantFlow.ok) {
  fail(`browser.flows.${REQUIRED_FLOW} is not ok`);
}

const missingFlowChecks = REQUIRED_CHECKS.filter((name) => !flowNames.has(name));
if (missingFlowChecks.length) {
  fail(`missing invariant flow checks: ${missingFlowChecks.join(', ')}`);
}

const missingAggregateChecks = REQUIRED_CHECKS.filter((name) => !aggregateNames.has(name));
if (missingAggregateChecks.length) {
  fail(`missing aggregated invariant checks: ${missingAggregateChecks.join(', ')}`);
}

console.log('Gateway smoke invariant gate passed.');
console.log(`- flow: ${REQUIRED_FLOW}`);
console.log(`- required checks: ${REQUIRED_CHECKS.length}`);
