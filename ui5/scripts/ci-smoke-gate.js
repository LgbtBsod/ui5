#!/usr/bin/env node
const childProcess = require('child_process');
const fs = require('fs');
const { readJsonSafe } = require('./lib/auditInput');
const { exitWithMappedIssues } = require('./lib/gate-result');

const reportPath = process.argv[2];

function loadReportFromFile(path) {
  if (!fs.existsSync(path)) {
    console.error(`Smoke report not found: ${path}`);
    process.exit(2);
  }

  const report = readJsonSafe(path, null);
  if (!report) {
    console.error(`Smoke report JSON is invalid: ${path}`);
    process.exit(2);
  }
  return report;
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
  } catch (_e) {
    const trimmed = String(result.stdout || '').trim();
    const firstBrace = trimmed.indexOf('{');
    const lastBrace = trimmed.lastIndexOf('}');
    if (firstBrace >= 0 && lastBrace > firstBrace) {
      try {
        return JSON.parse(trimmed.slice(firstBrace, lastBrace + 1));
      } catch (_ignored) {}
    }
    const lines = trimmed.split(/\r?\n/).filter(Boolean);
    const last = lines.length ? lines[lines.length - 1] : '';
    try {
      return JSON.parse(last);
    } catch (e) {
      if (result.status === 0) {
        return {
          status: 'ok',
          results: [{ name: 'unit-smoke', status: 'passed' }]
        };
      }
      return {
        status: 'error',
        results: [{ name: 'unit-smoke', status: 'failed', error: (result.stderr || result.stdout || e.message || 'unknown error').trim() }]
      };
    }
  }
}

const report = reportPath ? loadReportFromFile(reportPath) : runUnitSmokeAndCollectReport();
const results = Array.isArray(report.results) ? report.results : [];
const failed = results.filter((r) => r.status !== 'passed');

if (report.status !== 'ok' || failed.length > 0) {
  exitWithMappedIssues(
    'ci-smoke-gate',
    failed.map((f) => `${f.name}: ${f.error || 'unknown error'}`),
    (line) => ({ file: 'scripts/unit-smoke.js', message: String(line) }),
    { failed: failed.length, total: results.length },
    { asJson: process.argv.includes('--json') }
  );
}

exitWithMappedIssues('ci-smoke-gate', [], function (item) { return item; }, { failed: 0, total: results.length }, { asJson: process.argv.includes('--json') });
