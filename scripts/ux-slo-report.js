#!/usr/bin/env node
const fs = require('fs');

function argValue(flag) {
  const idx = process.argv.indexOf(flag);
  return idx >= 0 ? process.argv[idx + 1] : '';
}

const input = argValue('--input') || 'artifacts/ux-telemetry-sample.json';
const thresholds = JSON.parse(fs.readFileSync('scripts/ux-slo-thresholds.json', 'utf8'));
if (!fs.existsSync(input)) {
  console.error(`SLO report input not found: ${input}`);
  process.exit(2);
}
const events = JSON.parse(fs.readFileSync(input, 'utf8')).events || [];

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
