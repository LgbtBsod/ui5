#!/usr/bin/env node

const { readText, extractUi5Dependencies } = require('./qa-shared');
const { exitWithColonIssues } = require('./lib/issueGateRuntime');
const root = process.cwd();

const targets = [
  'controller/Search.controller.js',
  'controller/Detail.controller.js'
];

const allowed = [
  /^checklist\/app\/controller\/Base\.controller$/,
  /^checklist\/app\/controller\/support\/.+$/,
  /^checklist\/app\/service\/domain\/.+\/[^/]+Facade$/,
  /^checklist\/app\/service\/framework\/EffectApplier$/,
  /^checklist\/app\/service\/framework\/CtxFactory$/,
  /^checklist\/app\/util\/CreateSentinel$/,
  /^sap\/ui\/model\/json\/JSONModel$/
];

function main() {
  const violations = [];

  targets.forEach((file) => {
    const src = readText(root, file);
    const deps = extractUi5Dependencies(src).map((entry) => entry.dep);
    deps.forEach((dep) => {
      if (!allowed.some((rx) => rx.test(dep))) {
        violations.push(`${file}: disallowed import ${dep}`);
      }
    });
  });

  exitWithColonIssues(
    'controller-import-whitelist-gate',
    violations,
    { targetsScanned: targets.length, allowPatterns: allowed.length },
    { asJson: process.argv.includes('--json') }
  );
}

main();
