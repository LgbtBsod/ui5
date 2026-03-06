#!/usr/bin/env node

const path = require('path');
const { readJsonSafe, readTextSafe } = require('./lib/auditInput');
const { exitWithColonIssues } = require('./lib/issueGateRuntime');

const ROOT = process.cwd();

function modelKeys() {
  const manifest = readJsonSafe(path.join(ROOT, 'manifest.json'), null);
  if (!manifest) {
    throw new Error('manifest.json is missing or invalid JSON');
  }
  return Object.keys((((manifest || {})['sap.ui5'] || {}).models) || {});
}

function i18nKeys(file) {
  return readTextSafe(path.join(ROOT, file), '')
    .split(/\r?\n/)
    .filter((line) => line && !line.startsWith('#') && line.includes('='))
    .map((line) => line.split('=')[0]);
}

const requiredModels = ['mainService', 'i18n', 'data', 'state', 'layout', 'cache', 'masterData', 'mpl', 'uiState', 'selected'];
const issues = [];
const models = new Set(modelKeys());

requiredModels.forEach((model) => {
  if (!models.has(model)) {
    issues.push(`Missing manifest model: ${model}`);
  }
});

const baseKeys = new Set(i18nKeys('i18n/i18n.properties'));
const ruKeys = new Set(i18nKeys('i18n/i18n_ru.properties'));
const missingRu = [...baseKeys].filter((key) => !ruKeys.has(key));
const extraRu = [...ruKeys].filter((key) => !baseKeys.has(key));
if (missingRu.length) {
  issues.push(`Russian bundle misses ${missingRu.length} keys`);
}
if (extraRu.length) {
  issues.push(`Russian bundle contains ${extraRu.length} extra keys`);
}

const detailView = readTextSafe(path.join(ROOT, 'view/Detail.view.xml'), '');
if (!/selected>\/root/.test(detailView) || !/selected>\/basic/.test(detailView)) {
  issues.push('Detail.view.xml must bind editable detail data through selected model');
}

const saveUsecase = readTextSafe(path.join(ROOT, 'service/domain/detail/usecases/SaveDetailUseCase.js'), '');
if (!/get\("selected", "\/"\)/.test(saveUsecase)) {
  issues.push('SaveDetailUseCase must read selected model as primary current snapshot');
}

const autosaveUsecase = readTextSafe(path.join(ROOT, 'service/domain/detail/usecases/AutosaveDetailUseCase.js'), '');
if (!/get\("selected", "\/"\)/.test(autosaveUsecase)) {
  issues.push('AutosaveDetailUseCase must read selected model as primary current snapshot');
}

if (issues.length) {
  exitWithColonIssues(
    'domain-model-verify',
    issues,
    { requiredModels: requiredModels.length },
    { asJson: process.argv.includes('--json') },
    'manifest.json'
  );
}

exitWithColonIssues('domain-model-verify', [], { requiredModels: requiredModels.length }, { asJson: process.argv.includes('--json') }, 'manifest.json');
