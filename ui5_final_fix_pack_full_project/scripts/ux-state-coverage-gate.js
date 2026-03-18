#!/usr/bin/env node

const { missingPaths, readText, readJson, fail } = require('./lib/gateFs');

const planPath = 'docs/DEVELOPMENT_PLAN.md';
const baselinePath = 'docs/artifacts/ux-state-coverage.json';
if (missingPaths([planPath, baselinePath]).length) {
  fail('C1 gate failed: development plan or state coverage artifact is missing.');
}

const plan = readText(planPath);
if (!plan.includes('## WS-C. UX Governance, Accessibility, and Visual Consistency')) {
  fail('C1 gate failed: WS-C section is missing in DEVELOPMENT_PLAN.');
}

const baseline = readJson(baselinePath);
const requiredStates = ['loading', 'empty', 'error', 'conflict', 'permission'];
['search', 'detail'].forEach((flow) => {
  const states = (((baseline || {}).criticalFlows || {})[flow]) || [];
  requiredStates.forEach((state) => {
    if (!states.includes(state)) fail(`C1 gate failed: ${flow} missing state ${state}`);
  });
});

if (!baseline.visualBaselines || !baseline.visualBaselines.morning || !baseline.visualBaselines.night) {
  fail('C1 gate failed: Morning/Night visual baseline refs are required.');
}

console.log('C1 gate passed: WS-C plan and state coverage artifacts are present.');
