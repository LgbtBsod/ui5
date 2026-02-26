#!/usr/bin/env node
const fs = require('fs');

const path = 'docs/artifacts/pair-snapshots-baseline-matrix.json';
if (!fs.existsSync(path)) {
  console.error('Pair snapshots gate failed: baseline matrix is missing.');
  process.exit(1);
}

const report = JSON.parse(fs.readFileSync(path, 'utf8'));
const requiredFlows = ['search', 'detail', 'dialogs'];
const requiredModes = ['morning', 'night'];

for (const flow of requiredFlows) {
  const flowEntry = report.flows && report.flows[flow];
  if (!flowEntry) {
    console.error(`Pair snapshots gate failed: missing flow ${flow}`);
    process.exit(1);
  }

  for (const mode of requiredModes) {
    const modeEntry = flowEntry[mode];
    if (!modeEntry || typeof modeEntry.path !== 'string' || modeEntry.path.trim() === '') {
      console.error(`Pair snapshots gate failed: missing artifact path for ${flow}.${mode}`);
      process.exit(1);
    }
    if (modeEntry.status !== 'pass') {
      console.error(`Pair snapshots gate failed: ${flow}.${mode} status is not pass`);
      process.exit(1);
    }
  }
}

if (!report.summary || report.summary.coverage !== 1 || report.summary.pairedStates !== requiredFlows.length) {
  console.error('Pair snapshots gate failed: summary thresholds are not satisfied.');
  process.exit(1);
}

console.log('Pair snapshots gate passed: baseline matrix is complete for Morning/Night flows.');
