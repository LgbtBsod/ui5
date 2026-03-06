#!/usr/bin/env node
const fs = require('fs');
const { readJsonSafe, readTextSafe } = require('./lib/auditInput');
const { failWith } = require('./lib/reportGateRuntime');

const rfcPath = 'docs/artifacts/theme-preferences-contract-rfc.md';
const fixturesPath = 'scripts/backend-capability-fixtures.json';
const backendFiles = [
  'service/backend/FakeBackendService.js',
  'service/backend/RealBackendService.js'
];

if (!fs.existsSync(rfcPath)) {
  failWith('Theme preferences contract gate failed', 'RFC document is missing.', 1);
}

const rfcText = readTextSafe(rfcPath, '');
const requiredRFCSnippets = [
  '/user/theme-preferences',
  'themePreferences',
  'GET',
  'PUT'
];
for (const snippet of requiredRFCSnippets) {
  if (!rfcText.includes(snippet)) {
    failWith('Theme preferences contract gate failed', `RFC missing required content: ${snippet}`, 1);
  }
}

if (!fs.existsSync(fixturesPath)) {
  failWith('Theme preferences contract gate failed', 'capability fixtures are missing.', 1);
}

const fixtures = readJsonSafe(fixturesPath, null);
if (!fixtures || typeof fixtures !== 'object') {
  failWith('Theme preferences contract gate failed', 'capability fixtures JSON is invalid.', 1);
}
const requiredFeatures = fixtures.requiredFeatures || [];
if (!requiredFeatures.includes('themePreferences')) {
  failWith('Theme preferences contract gate failed', 'capability fixtures must require themePreferences feature.', 1);
}

for (const backendFile of backendFiles) {
  if (!fs.existsSync(backendFile)) {
    failWith('Theme preferences contract gate failed', `backend file missing: ${backendFile}`, 1);
  }
  const text = readTextSafe(backendFile, '');
  if (!/themePreferences\s*:\s*true/.test(text)) {
    failWith('Theme preferences contract gate failed', `${backendFile} does not advertise themePreferences capability.`, 1);
  }
}

console.log('Theme preferences contract gate passed: RFC and capability flags are in place.');
