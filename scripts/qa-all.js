#!/usr/bin/env node

const path = require('path');
const fs = require('fs');
const { runNodeScript } = require('./qa-shared');
const {
  validators,
  getOptionalValidators
} = require('./lib/qa-pipeline-config');
const verbose = process.argv.includes('--verbose');
const rootDir = path.resolve(__dirname, '..');

function runValidator(validator, index, total) {
  const fileSpec = String(validator.file || '').trim();
  const firstToken = fileSpec.split(/\s+/)[0] || '';
  const scriptPath = path.join(__dirname, firstToken);
  if (!fs.existsSync(scriptPath)) return { status: 1, output: `Missing validator script: ${scriptPath}` };
  console.log(`\n[${index + 1}/${total}] ${validator.name}`);
  const result = runNodeScript(rootDir, `scripts/${fileSpec}`);
  if (result.status !== 0) {
    const advisory = validator.mode === 'advisory';
    console.log(advisory ? 'WARN' : 'FAIL');
    console.log(result.output || '(no output)');
    return { status: advisory ? 0 : (result.status || 1), advisoryFailed: advisory };
  }
  console.log('PASS');
  if (verbose && result.output) console.log(result.output);
  return { status: 0, advisoryFailed: false };
}

(function main() {
  const optional = getOptionalValidators(process.env);
  let advisoryFailures = 0;
  console.log('====================================\nUI5 ENTERPRISE QA PIPELINE\n====================================');
  const total = validators.length;
  for (let i = 0; i < total; i += 1) {
    const result = runValidator(validators[i], i, total);
    if (result.advisoryFailed) advisoryFailures += 1;
    if (result.status !== 0) process.exit(result.status);
  }
  optional.filter((item) => item.enabled).forEach((item) => {
    const result = runValidator(item, 0, 1);
    if (result.status !== 0) console.log(`WARN optional validator failed: ${item.name}`);
  });
  console.log('\n------------------------------------');
  console.log('HARD CONTRACT CHECKS PASSED');
  if (advisoryFailures > 0) {
    console.log(`ADVISORY FINDINGS: ${advisoryFailures}`);
  }
  console.log('------------------------------------');
})();
