#!/usr/bin/env node

const { runTextContractGate } = require('./lib/artifactContractGate');

const requiredVars = ['--sapPositive', '--sapNegative', '--sapCritical', '--sapInformation'];

function validateSemanticContrast(css) {
  const issues = [];
  const missing = requiredVars.filter(function (value) { return !css.includes(value); });
  if (missing.length) {
    issues.push(`Semantic contrast gate failed: missing semantic vars: ${missing.join(', ')}`);
  }
  if (!css.includes(':root.light-mode') || !css.includes('body.appDark')) {
    issues.push('Semantic contrast gate failed: missing Morning/Night theme scopes.');
  }
  return issues;
}

const contract = {
  dataPath: 'css/style.css',
  missingMessage: 'Semantic contrast gate failed: css/style.css not found.',
  passMessage: 'Semantic contrast gate passed: semantic variables and dual theme scopes detected.',
  validate: validateSemanticContrast
};

runTextContractGate(contract);
