#!/usr/bin/env node

const qa = require('./qa-shared');
const { exitWithColonIssues } = require('./lib/issueGateRuntime');

const root = process.cwd();
const files = qa.collectFilesByExtensions(root, ['controller'], ['.js']);

const FORBIDDEN_IMPORTS = [
  'sap/m/MessageBox',
  'sap/m/MessageToast',
  'sap/ui/model/odata'
];
const FORBIDDEN_SUBSTRINGS = [
  '/service/backend/',
  'sap/ui/model/odata'
];
const violations = [];

function push(file, line, rule, sample) {
  violations.push(`${file}:${line || '?'} ${rule}${sample ? ` :: ${sample}` : ''}`);
}

for (const file of files) {
  const src = qa.readText(root, file);
  const deps = qa.extractUi5Dependencies(src);

  for (const dep of deps) {
    if (FORBIDDEN_IMPORTS.some((needle) => dep.dep === needle || dep.dep.startsWith(needle))) {
      push(file, qa.lineFromIndex(src, dep.index), 'forbidden-import', dep.dep);
    }
    if (dep.dep.includes('/service/backend/')) {
      push(file, qa.lineFromIndex(src, dep.index), 'backend-import', dep.dep);
    }
  }

  const lines = src.split(/\r?\n/);
  lines.forEach((line, idx) => {
    const n = idx + 1;
    FORBIDDEN_SUBSTRINGS.forEach((needle) => {
      if (line.includes(needle)) {
        push(file, n, 'forbidden-reference', needle);
      }
    });

    if (/\bMessageBox\s*\./.test(line)) {
      push(file, n, 'direct-messagebox-call', line.trim());
    }
    if (/\bMessageToast\s*\./.test(line)) {
      push(file, n, 'direct-messagetoast-call', line.trim());
    }
  });
}

exitWithColonIssues(
  'controller-purity-gate',
  violations,
  { filesScanned: files.length },
  { asJson: process.argv.includes('--json') }
);
