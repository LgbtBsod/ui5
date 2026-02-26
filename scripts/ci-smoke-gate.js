#!/usr/bin/env node
const childProcess = require('child_process');
const fs = require('fs');

const reportPath = process.argv[2];

function loadReportFromFile(path) {
  if (!fs.existsSync(path)) {
    console.error(`Smoke report not found: ${path}`);
    process.exit(2);
  }

  return JSON.parse(fs.readFileSync(path, 'utf8'));
}

function runUnitSmokeAndCollectReport() {
  const result = childProcess.spawnSync('node', ['scripts/unit-smoke.js', '--json'], {
    encoding: 'utf8'
  });

  if (result.error) {
    console.error(`Smoke gate failed to execute unit smoke: ${result.error.message}`);
    process.exit(2);
  }

  if (!result.stdout || !result.stdout.trim()) {
    console.error('Smoke gate received an empty unit smoke report.');
    if (result.stderr) {
      console.error(result.stderr.trim());
    }
    process.exit(2);
  }

  try {
    return JSON.parse(result.stdout);
  } catch (e) {
    console.error(`Smoke gate could not parse unit smoke JSON output: ${e.message}`);
    if (result.stderr) {
      console.error(result.stderr.trim());
    }
    process.exit(2);
  }
}

const report = reportPath ? loadReportFromFile(reportPath) : runUnitSmokeAndCollectReport();
const results = Array.isArray(report.results) ? report.results : [];
const failed = results.filter((r) => r.status !== 'passed');

if (report.status !== 'ok' || failed.length > 0) {
  console.error(`Smoke gate failed: ${failed.length} test(s) failed.`);
  failed.forEach((f) => console.error(`- ${f.name}: ${f.error || 'unknown error'}`));
  process.exit(1);
}

console.log(`Smoke gate passed: ${results.length} test(s).`);
