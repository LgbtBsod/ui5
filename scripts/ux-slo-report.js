#!/usr/bin/env node
const { requireJsonReport } = require('./lib/reportGateRuntime');
const { argValue } = require('./lib/cliArgs');

const input = argValue(process.argv, '--input', '') || 'artifacts/ux-telemetry-sample.json';
const thresholds = requireJsonReport('scripts/ux-slo-thresholds.json', {
  prefix: 'SLO report failed',
  missingExitCode: 2,
  invalidExitCode: 2
});
const telemetry = requireJsonReport(input, {
  prefix: 'SLO report failed',
  missingExitCode: 2,
  invalidExitCode: 2
});
const events = telemetry.events || [];

function p95(list) {
  if (!list.length) return 0;
  const sorted = list.slice().sort((a, b) => a - b);
  return sorted[Math.max(0, Math.ceil(sorted.length * 0.95) - 1)];
}

let failed = false;
let warned = false;
Object.keys(thresholds).forEach((op) => {
  const vals = events.filter((e) => e.operation === op).map((e) => Number(e.durationMs || 0));
  const value = p95(vals);
  const t = thresholds[op];
  if (value > t.failP95Ms) {
    failed = true;
    console.error(`[FAIL] ${op} p95=${value}ms > ${t.failP95Ms}ms`);
  } else if (value > t.warnP95Ms) {
    warned = true;
    console.log(`[WARN] ${op} p95=${value}ms > ${t.warnP95Ms}ms`);
  } else {
    console.log(`[PASS] ${op} p95=${value}ms`);
  }
});
if (failed) process.exit(1);
if (warned) process.exit(0);
