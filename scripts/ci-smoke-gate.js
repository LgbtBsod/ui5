#!/usr/bin/env node
const fs = require('fs');

const path = process.argv[2] || '/tmp/unit-smoke-report.json';
if (!fs.existsSync(path)) {
  console.error(`Smoke report not found: ${path}`);
  process.exit(2);
}

const report = JSON.parse(fs.readFileSync(path, 'utf8'));
const results = Array.isArray(report.results) ? report.results : [];
const failed = results.filter((r) => r.status !== 'passed');

if (report.status !== 'ok' || failed.length > 0) {
  console.error(`Smoke gate failed: ${failed.length} test(s) failed.`);
  failed.forEach((f) => console.error(`- ${f.name}: ${f.error || 'unknown error'}`));
  process.exit(1);
}

console.log(`Smoke gate passed: ${results.length} test(s).`);
