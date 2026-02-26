#!/usr/bin/env node
const fs = require('fs');

const checklistPath = 'docs/ux/design-governance-checklist.md';
const baselinePath = 'docs/ux/baselines/state-coverage.json';
if (!fs.existsSync(checklistPath) || !fs.existsSync(baselinePath)) {
  console.error('C1 gate failed: checklist or state coverage artifact is missing.');
  process.exit(1);
}
const baseline = JSON.parse(fs.readFileSync(baselinePath, 'utf8'));
const requiredStates = ['loading', 'empty', 'error', 'conflict', 'permission'];
['search', 'detail'].forEach((flow) => {
  const states = (((baseline || {}).criticalFlows || {})[flow]) || [];
  requiredStates.forEach((state) => {
    if (!states.includes(state)) {
      console.error(`C1 gate failed: ${flow} missing state ${state}`);
      process.exit(1);
    }
  });
});
if (!baseline.visualBaselines || !baseline.visualBaselines.morning || !baseline.visualBaselines.night) {
  console.error('C1 gate failed: Morning/Night visual baseline refs are required.');
  process.exit(1);
}
console.log('C1 gate passed: checklist, state coverage and visual baseline refs are present.');
