#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { runNodeScript } = require('./qa-shared');
const { getChangedFiles } = require('./lib/git-changes');
const { scanFile } = require('./lib/js-deps-scanner');
const { argValue, hasArg } = require('./lib/cliArgs');

function toError(message) {
  return {
    ruleId: 'QA_PIPELINE', severity: 'error', file: 'scripts/qa-all.js', line: 1,
    message, evidence: message,
    fixHint: 'Run failing gate directly and address reported violation.',
    goodExample: 'npm run qa -> ALL ARCHITECTURE CHECKS PASSED',
    badExample: 'npm run qa -> FAIL <gate>', suggestedPatch: ''
  };
}

function buildResult(name, ok, output) {
  return { name, ok, errors: ok ? [] : [toError(output)], stats: { outputLines: String(output || '').split(/\r?\n/).length } };
}

function runQa() {
  const result = runNodeScript(process.cwd(), 'scripts/qa-all.js');
  return buildResult('qa-all', result.status === 0, result.output || '');
}

function buildChangedDeps(files) {
  const rows = [];
  files.filter((f) => f.endsWith('.js') && fs.existsSync(f)).forEach((file) => {
    rows.push({ file, deps: scanFile(file).map((d) => d.dep) });
  });
  return rows;
}

function writeReport(file, qaResult, changedDeps) {
  const lines = [
    '# QA report (latest)',
    '',
    '## Gate summary',
    '',
    `- Gate: **${qaResult.name}**`,
    `- Status: **${qaResult.ok ? 'PASS' : 'FAIL'}**`,
    `- Error count: **${qaResult.errors.length}**`,
    ''
  ];

  lines.push('## Top offenders', '');
  if (!qaResult.errors.length) {
    lines.push('- None.');
  } else {
    qaResult.errors.slice(0, 10).forEach((e) => {
      lines.push(`- ${e.file}:${e.line} :: ${e.message}`);
    });
  }
  lines.push('');

  lines.push('## Fix hints', '');
  if (!qaResult.errors.length) {
    lines.push('- No fixes required; all gates passed.');
  } else {
    qaResult.errors.slice(0, 10).forEach((e) => {
      lines.push(`- **${e.ruleId}**: ${e.fixHint}`);
      lines.push(`  - Good: ${e.goodExample}`);
      lines.push(`  - Bad: ${e.badExample}`);
      if (e.suggestedPatch) lines.push(`  - Patch: ${e.suggestedPatch}`);
    });
  }
  lines.push('');

  lines.push('## Changed JS dependency scan', '');
  if (!changedDeps.length) {
    lines.push('- No changed JS files detected.');
  } else {
    changedDeps.forEach((row) => {
      lines.push('- `'+row.file+'`');
      row.deps.slice(0, 12).forEach((d) => lines.push(`  - ${d}`));
    });
  }
  lines.push('');

  lines.push('## Documentation links', '');
  lines.push('- `docs/LOCAL_VALIDATION.md`');
  lines.push('- `docs/generated-artifacts.md`');
  lines.push('- `docs/SAP_SALE_READINESS_STATUS.md`');

  fs.mkdirSync(path.dirname(file), { recursive: true });
  fs.writeFileSync(file, lines.join('\n'));
}

(function main() {
  const changedOnly = hasArg(process.argv, '--changed');
  const asJson = hasArg(process.argv, '--json');
  const failfast = hasArg(process.argv, '--failfast');
  const reportPath = argValue(process.argv, '--report', null) || 'docs/qa-report-latest.md';

  const qaResult = runQa();
  const changedFiles = changedOnly ? getChangedFiles() : [];
  const changedDeps = buildChangedDeps(changedFiles);
  const payload = { generatedAt: new Date().toISOString(), gates: [qaResult], changedFiles, changedDeps, failfast };

  writeReport(reportPath, qaResult, changedDeps);
  if (asJson) console.log(JSON.stringify(payload, null, 2));
  else console.log(`${qaResult.ok ? 'PASS' : 'FAIL'} qa-runner`);
  process.exit(qaResult.ok ? 0 : 1);
})();
