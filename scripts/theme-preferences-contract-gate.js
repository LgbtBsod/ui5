#!/usr/bin/env node
const fs = require('fs');

const rfcPath = 'docs/artifacts/theme-preferences-contract-rfc.md';
const fixturesPath = 'scripts/backend-capability-fixtures.json';
const backendFiles = [
  'service/backend/FakeBackendService.js',
  'service/backend/RealBackendService.js'
];

if (!fs.existsSync(rfcPath)) {
  console.error('Theme preferences contract gate failed: RFC document is missing.');
  process.exit(1);
}

const rfcText = fs.readFileSync(rfcPath, 'utf8');
const requiredRFCSnippets = [
  '/user/theme-preferences',
  'themePreferences',
  'GET',
  'PUT'
];
for (const snippet of requiredRFCSnippets) {
  if (!rfcText.includes(snippet)) {
    console.error(`Theme preferences contract gate failed: RFC missing required content: ${snippet}`);
    process.exit(1);
  }
}

if (!fs.existsSync(fixturesPath)) {
  console.error('Theme preferences contract gate failed: capability fixtures are missing.');
  process.exit(1);
}

const fixtures = JSON.parse(fs.readFileSync(fixturesPath, 'utf8'));
const requiredFeatures = fixtures.requiredFeatures || [];
if (!requiredFeatures.includes('themePreferences')) {
  console.error('Theme preferences contract gate failed: capability fixtures must require themePreferences feature.');
  process.exit(1);
}

for (const backendFile of backendFiles) {
  if (!fs.existsSync(backendFile)) {
    console.error(`Theme preferences contract gate failed: backend file missing: ${backendFile}`);
    process.exit(1);
  }
  const text = fs.readFileSync(backendFile, 'utf8');
  if (!/themePreferences\s*:\s*true/.test(text)) {
    console.error(`Theme preferences contract gate failed: ${backendFile} does not advertise themePreferences capability.`);
    process.exit(1);
  }
}

console.log('Theme preferences contract gate passed: RFC and capability flags are in place.');
