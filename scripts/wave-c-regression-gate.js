#!/usr/bin/env node
const fs = require('fs');

function fail(msg) {
  console.error(msg);
  process.exit(1);
}

const reportPath = process.argv[2];
if (!reportPath) {
  fail('Usage: node scripts/wave-c-regression-gate.js /path/to/unit-smoke-report.json');
}

let report;
try {
  report = JSON.parse(fs.readFileSync(reportPath, 'utf8'));
} catch (e) {
  fail(`Failed to read JSON report: ${e.message}`);
}

const results = Array.isArray(report) ? report : report && Array.isArray(report.results) ? report.results : null;
if (!results) {
  fail('Report must be an array of test results or an object with results[]');
}

const failed = results.filter((r) => r.status !== 'passed');
if (failed.length > 0) {
  fail(`Wave C regression gate failed: ${failed.length} test(s) are not passed.`);
}

const required = [
  'WaveB3CriticalJourneysMatrix',
  'DetailSaveConflictUseCase',
  'DetailEditOrchestrationUseCase',
  'SearchRetryMessagePresentationUseCase',
  'SearchActionMessagePresentationUseCase',
  'OperationalKpiInstrumentationUseCase'
];

const names = new Set(results.map((r) => r.name));
const missing = required.filter((name) => !names.has(name));
if (missing.length > 0) {
  fail(`Wave C regression gate failed: missing required tests: ${missing.join(', ')}`);
}

console.log(`Wave C regression gate passed: ${results.length} smoke test(s), required matrix present.`);
