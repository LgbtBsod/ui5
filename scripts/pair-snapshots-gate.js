#!/usr/bin/env node
const { requireJsonReport, failWith } = require('./lib/reportGateRuntime');

const path = 'docs/artifacts/pair-snapshots-baseline-matrix.json';
const MAX_AGE_DAYS = 14;
const report = requireJsonReport(path, {
  prefix: 'Pair snapshots gate failed',
  missingExitCode: 1,
  invalidExitCode: 1
});
const requiredFlows = ['search', 'detail', 'dialogs'];
const requiredModes = ['morning'];
const optionalModes = ['night'];

for (const flow of requiredFlows) {
  const flowEntry = report.flows && report.flows[flow];
  if (!flowEntry) {
    failWith('Pair snapshots gate failed', `missing flow ${flow}`, 1);
  }

  for (const mode of requiredModes) {
    const modeEntry = flowEntry[mode];
    if (!modeEntry || typeof modeEntry.path !== 'string' || modeEntry.path.trim() === '') {
      failWith('Pair snapshots gate failed', `missing artifact path for ${flow}.${mode}`, 1);
    }
    if (modeEntry.status !== 'pass') {
      failWith('Pair snapshots gate failed', `${flow}.${mode} status is not pass`, 1);
    }
  }

  for (const mode of optionalModes) {
    const modeEntry = flowEntry[mode];
    if (!modeEntry) {
      continue;
    }
    if (typeof modeEntry.path !== 'string' || modeEntry.path.trim() === '') {
      failWith('Pair snapshots gate failed', `artifact path for ${flow}.${mode} is empty`, 1);
    }
    if (modeEntry.status !== 'pass') {
      failWith('Pair snapshots gate failed', `${flow}.${mode} status is not pass`, 1);
    }
  }
}

if (!report.summary || report.summary.coverage !== 1) {
  failWith('Pair snapshots gate failed', 'summary thresholds are not satisfied.', 1);
}

if (!report.generatedAt) {
  failWith('Pair snapshots gate failed', 'generatedAt is missing.', 1);
}

const generatedAt = new Date(String(report.generatedAt).replace(' ', 'T'));
if (Number.isNaN(generatedAt.getTime())) {
  failWith('Pair snapshots gate failed', 'generatedAt is invalid.', 1);
}

const ageMs = Date.now() - generatedAt.getTime();
const maxAgeMs = MAX_AGE_DAYS * 24 * 60 * 60 * 1000;
if (ageMs > maxAgeMs) {
  failWith('Pair snapshots gate failed', `baseline matrix is older than ${MAX_AGE_DAYS} days.`, 1);
}

console.log('Pair snapshots gate passed: baseline matrix is complete for productive morning flows.');
