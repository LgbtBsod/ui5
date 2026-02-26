#!/usr/bin/env node
const fs = require('fs');

const planPath = 'docs/DEVELOPMENT_PLAN.md';
const baselinePath = 'docs/artifacts/ux-state-coverage.json';
if (!fs.existsSync(planPath) || !fs.existsSync(baselinePath)) {
  console.error('C1 gate failed: development plan or state coverage artifact is missing.');
  process.exit(1);
}

const plan = fs.readFileSync(planPath, 'utf8');
if (!plan.includes('## WS-C. UX Governance, Accessibility, and Visual Consistency')) {
  console.error('C1 gate failed: WS-C section is missing in DEVELOPMENT_PLAN.');
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
console.log('C1 gate passed: WS-C plan and state coverage artifacts are present.');
