#!/usr/bin/env node

const path = require('path');
const { requireJsonReport, failWith } = require('./lib/reportGateRuntime');

const ROOT = process.cwd();
const reportPath = path.resolve(ROOT, process.argv[2] || path.join('docs', 'artifacts', 'gateway-live-evidence-summary.json'));
const REQUIRED_CLASSES = ['PASS_SAP_EVIDENCE', 'BLOCKED_SAP_ENV', 'FAIL_PRODUCT_CONTRACT'];

const report = requireJsonReport(reportPath, {
  prefix: 'Gateway live evidence gate failed',
  missingExitCode: 2
});

const failures = [];
if (!REQUIRED_CLASSES.includes(String(report.resultClass || ''))) {
  failures.push(`invalid resultClass: ${String(report.resultClass || '')}`);
}
if (!String(report.environmentUsed || '').includes('/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV')) {
  failures.push('environmentUsed must point at SAP Gateway service root');
}
if (/127\.0\.0\.1|localhost/i.test(String(report.environmentUsed || ''))) {
  failures.push('environmentUsed must not point at localhost for live evidence');
}
if (!Array.isArray(report.executedScripts)) {
  failures.push('executedScripts must be an array');
}
if (report.resultClass !== 'BLOCKED_SAP_ENV' && (!Array.isArray(report.executedScripts) || !report.executedScripts.length)) {
  failures.push('executedScripts must be present for non-blocked runs');
}
if (!Array.isArray(report.producedArtifacts) || !report.producedArtifacts.length) {
  failures.push('producedArtifacts must be present');
}

if (failures.length) {
  failWith('Gateway live evidence gate failed', failures.join('; '), 1);
}

console.log('Gateway live evidence gate passed.');
console.log(`- resultClass: ${report.resultClass}`);
console.log(`- environment: ${report.environmentUsed}`);
