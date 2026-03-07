#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { collectFilesByExtensions } = require('../qa-shared');
const { exitWithColonIssues } = require('../lib/issueGateRuntime');
const { readJsonSafe } = require('../lib/auditInput');

const root = path.resolve(__dirname, '..', '..');
const files = collectFilesByExtensions(root, ['.', 'controller', 'service', 'util', 'infra'], ['.js'])
  .filter((file) => !file.startsWith('scripts/'))
  .filter((file) => !file.startsWith('backend/mock_gateway/'))
  .filter((file) => !file.startsWith('mock_gate_way/'))
  .filter((file) => !file.startsWith('node_modules/'));

const allowFiles = new Set([
  'app/Component.js',
  'app/model/ModelFactory.js',
  'Component.js',
  'model/ModelFactory.js'
]);

const forbiddenPatternsPath = path.join(root, 'scripts', 'ci', 'uiState-workflow-forbidden-patterns.json');
const forbiddenPatterns = readJsonSafe(forbiddenPatternsPath, []);

const issues = [];

files.forEach((file) => {
  if (allowFiles.has(file)) {
    return;
  }
  const abs = path.join(root, file);
  const text = fs.readFileSync(abs, 'utf8');
  if (!/\buiState\b/.test(text) && !/["']uiState["']/.test(text)) {
    return;
  }

  forbiddenPatterns.forEach((pattern) => {
    const escapedPath = pattern.path.replace(/\//g, '\\/');
    const uiStateFacadeAccess = new RegExp(`(?:get|set)\\s*\\(\\s*["']uiState["']\\s*,\\s*["']${escapedPath}["']`, 'g');
    const uiStateModelAccess = new RegExp(`uiStateModel\\s*\\.\\s*(?:getProperty|setProperty)\\s*\\(\\s*["']${escapedPath}["']`, 'g');
    const directUiStateVarAccess = new RegExp(`oUiState(?:Model)?\\s*\\.\\s*(?:getProperty|setProperty)\\s*\\(\\s*["']${escapedPath}["']`, 'g');
    const getModelUiStateAccess = new RegExp(`getModel\\s*\\(\\s*["']uiState["']\\s*\\)\\s*\\.\\s*(?:getProperty|setProperty)\\s*\\(\\s*["']${escapedPath}["']`, 'g');
    if (uiStateFacadeAccess.test(text) || uiStateModelAccess.test(text) || directUiStateVarAccess.test(text) || getModelUiStateAccess.test(text)) {
      issues.push(`${file}: workflow state mirror access detected for ${pattern.label}; keep canonical workflow state in state model only`);
    }
  });
});

exitWithColonIssues('uiState-workflow-mirror-gate', issues, { filesScanned: files.length, allowFiles: allowFiles.size }, { asJson: process.argv.includes('--json') });
