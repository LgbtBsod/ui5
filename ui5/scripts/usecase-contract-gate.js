#!/usr/bin/env node

const { collectFilesByExtensions, readText } = require('./qa-shared');
const { exitWithColonIssues } = require('./lib/issueGateRuntime');

const root = process.cwd();
const files = collectFilesByExtensions(root, ['service/domain'], ['.js']).filter((f) => f.includes('/usecases/'));
const violations = [];

for (const file of files) {
  const src = readText(root, file);
  const hasObjectExecute = /execute\s*:\s*function\s*\(/.test(src);
  const hasPrototypeExecute = /\.prototype\.execute\s*=\s*function\s*\(/.test(src);
  if (!hasObjectExecute && !hasPrototypeExecute) {
    violations.push(`${file}: missing execute(input, ctx) contract`);
  }
}

exitWithColonIssues('usecase-contract-gate', violations, { filesScanned: files.length }, { asJson: process.argv.includes('--json') });
