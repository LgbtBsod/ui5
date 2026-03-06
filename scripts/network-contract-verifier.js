#!/usr/bin/env node

const { parseArgs, runVerifier } = require('./lib/networkContractVerifierCore');

(function main() {
  const rootDir = process.cwd();
  const args = parseArgs(process.argv.slice(2));
  const result = runVerifier(rootDir, args);
  const summary = result.summary;
  const lines = [
    'NETWORK CONTRACT VERIFIER',
    '---',
    `mode: ${result.mode.toUpperCase()}`,
    `metadata: ${summary.metadata ? 'PASS' : 'FAIL'}`,
    `batch list: ${summary.batchList ? 'PASS' : 'FAIL'}`,
    `segments: ${summary.segments ? 'PASS' : 'FAIL'}`,
    `no expand: ${summary.noExpand ? 'PASS' : 'FAIL'}`,
    `no REST: ${summary.noRest ? 'PASS' : 'FAIL'}`,
    `duplicates: ${summary.duplicateStatus === 'SKIPPED' ? 'SKIPPED' : (summary.duplicateViolations.length ? 'FAIL' : 'PASS')}`,
    '---',
    `OVERALL: ${summary.overall ? 'PASS' : 'FAIL'}`
  ];
  console.log(lines.join('\n'));

  if (result.verbose) {
    if (result.sourcePath) {
      console.log(`traceSource: ${result.sourcePath}`);
    }
    if (summary.duplicateStatus === 'SKIPPED') {
      console.log('duplicateDetection: skipped (intent mode)');
    } else {
      console.log(`duplicateDetection: checked (${summary.duplicateViolations.length} violations, ${summary.duplicateWarnings.length} warnings)`);
    }
  }

  if (!summary.overall) {
    summary.failures.slice(0, 5).forEach((f) => console.log(`- ${f}`));
    process.exit(1);
  }
})();
