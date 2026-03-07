#!/usr/bin/env node

const {
  normalizePath,
  fileExists,
  readText,
  collectFilesByExtensions,
  scanRegexInFiles
} = require('../qa-shared');
const { getChangedFiles } = require('../lib/git-changes');

const ROOT = process.cwd();
const REQUIRED_SERVICE_ROOT = '/sap/opu/odata/sap/Z_UI5_SRV/';
const MANIFEST_PATH = 'manifest.json';
const COMPONENT_PATH = 'Component.js';

function readManifestSafe(fails, ruleId) {
  if (!fileExists(ROOT, MANIFEST_PATH)) {
    return null;
  }

  try {
    return JSON.parse(readText(ROOT, MANIFEST_PATH));
  } catch (error) {
    addIssue(fails, ruleId, MANIFEST_PATH, `manifest parse error: ${error.message}`);
    return null;
  }
}

function getRuntimeFiles() {
  const files = collectFilesByExtensions(ROOT, ['controller', 'service', 'infra', 'util', 'view'], ['.js', '.json', '.xml']);

  for (const file of ['Component.js', 'manifest.json']) {
    if (fileExists(ROOT, file)) {
      files.push(file);
    }
  }

  return [...new Set(files)].sort();
}

function addIssue(target, rule, file, hint, line) {
  target.push({ rule, file, hint, line: line || null });
}

function checkForbiddenFallbackNetwork(fails, runtimeFiles) {
  const rules = [
    { id: 'A', regex: /fetch\s*\(/, hint: 'fetch() is forbidden in runtime code' },
    { id: 'A', regex: /\bnew\s+XMLHttpRequest\s*\(/, hint: 'XMLHttpRequest is forbidden in runtime code' },
    { id: 'A', regex: /\baxios\b/, hint: 'axios is forbidden in runtime code' }
  ];

  for (const rule of rules) {
    scanRegexInFiles(ROOT, runtimeFiles, rule.regex, (file, source, match, line) => {
      addIssue(fails, rule.id, file, rule.hint, line);
    });
  }
}

function checkForbiddenExpand(fails, runtimeFiles) {
  scanRegexInFiles(ROOT, runtimeFiles, /\$expand|expand=/, (file, source, match, line) => {
    addIssue(fails, 'B', file, 'Found forbidden $expand/expand=', line);
  });
}

function checkForbiddenFlexibleRouter(fails, runtimeFiles) {
  const checks = [
    /routerClass[^\n]*FlexibleRouter/,
    /setFclControlId/,
    /FlexibleRouter/
  ];

  for (const re of checks) {
    scanRegexInFiles(ROOT, runtimeFiles, re, (file, source, match, line) => {
      addIssue(fails, 'C', file, 'FlexibleRouter/FCL usage is forbidden', line);
    });
  }
}

function checkServiceRootContract(fails) {
  let found = false;

  const manifest = readManifestSafe(fails, 'D');
  if (manifest) {
    const appMain = ((((manifest['sap.app'] || {}).dataSources || {}).mainService || {}).uri) || '';
    const ui5Main = ((((manifest['sap.ui5'] || {}).dataSources || {}).mainService || {}).uri) || '';
    if (appMain.includes(REQUIRED_SERVICE_ROOT) || ui5Main.includes(REQUIRED_SERVICE_ROOT)) {
      found = true;
    }
  }

  if (!found && fileExists(ROOT, COMPONENT_PATH)) {
    const src = readText(ROOT, COMPONENT_PATH);
    const index = src.indexOf(REQUIRED_SERVICE_ROOT);
    if (index >= 0) {
      found = true;
    }
  }

  if (!found) {
    addIssue(fails, 'D', MANIFEST_PATH, `Required service root not found: ${REQUIRED_SERVICE_ROOT}`);
  }
}

function checkODataV2AndUseBatch(fails) {
  let hasV2 = false;
  let hasUseBatch = false;

  const manifest = readManifestSafe(fails, 'E');
  if (manifest) {
    const models = ((manifest['sap.ui5'] || {}).models) || {};
    for (const modelName of Object.keys(models)) {
      const model = models[modelName] || {};
      if (model.type === 'sap.ui.model.odata.v2.ODataModel') {
        hasV2 = true;
      }
      if (model.settings && model.settings.useBatch === true) {
        hasUseBatch = true;
      }
    }
  }

  if (fileExists(ROOT, COMPONENT_PATH)) {
    const source = readText(ROOT, COMPONENT_PATH);
    if (/sap\.ui\.model\.odata\.v2\.ODataModel/.test(source) || /new\s+ODataModel\s*\(/.test(source)) {
      hasV2 = true;
    }
    if (/useBatch\s*:\s*true/.test(source)) {
      hasUseBatch = true;
    }
  }

  if (!hasV2) {
    addIssue(fails, 'E', MANIFEST_PATH, 'OData V2 model contract not found');
  }
  if (!hasUseBatch) {
    addIssue(fails, 'E', MANIFEST_PATH, 'useBatch: true contract not found');
  }
}

function findSearchViewFile() {
  const preferred = 'view/Search.view.xml';
  if (fileExists(ROOT, preferred)) {
    return preferred;
  }

  const viewFiles = collectFilesByExtensions(ROOT, ['view'], ['.xml']);
  return viewFiles.find((file) => /searchSmartTable/.test(readText(ROOT, file))) || null;
}

function checkSearchViewContract(fails) {
  const viewFile = findSearchViewFile();
  if (!viewFile) {
    addIssue(fails, 'F', 'view/', 'Search view not found (expected SmartTable contract)');
    return;
  }

  const source = readText(ROOT, viewFile);
  const requiredPatterns = [
    { regex: /id="searchSmartFilterBar"/, hint: 'Missing id="searchSmartFilterBar"' },
    { regex: /id="searchSmartTable"/, hint: 'Missing id="searchSmartTable"' },
    { regex: /entitySet="ChecklistSearchSet"/, hint: 'Missing entitySet="ChecklistSearchSet"' },
    { regex: /beforeRebindTable\s*=\s*"\.?onBeforeSmartTableRebind"/, hint: 'Missing beforeRebindTable="onBeforeSmartTableRebind"' }
  ];

  for (const entry of requiredPatterns) {
    const match = entry.regex.exec(source);
    if (!match) {
      addIssue(fails, 'F', viewFile, entry.hint);
    }
  }
}

function checkSegmentFiltersContract(fails) {
  const files = [
    'controller/Search.controller.js',
    'util/search/SearchFilterBuilder.js'
  ].filter((file) => fileExists(ROOT, file));

  if (!files.length) {
    addIssue(fails, 'G', 'controller/Search.controller.js', 'Search contract files not found');
    return;
  }

  const joined = files.map((file) => readText(ROOT, file)).join('\n');

  for (const token of ['HasFailedChecks', 'HasFailedBarriers']) {
    if (!joined.includes(token)) {
      addIssue(fails, 'G', files[0], `Missing segment filter token: ${token}`);
    }
  }

  for (const key of ['ALL', 'FAILED', 'SUCCESS']) {
    if (!new RegExp(`["']${key}["']`).test(joined)) {
      addIssue(fails, 'G', files[0], `Missing segment key: ${key}`);
    }
  }
}

function checkCreateModeContract(fails, warnings) {
  if (!fileExists(ROOT, COMPONENT_PATH)) {
    addIssue(warnings, 'H', COMPONENT_PATH, 'Component.js not found to validate __create contract');
    return;
  }

  const source = readText(ROOT, COMPONENT_PATH);
  const hasCreate = source.includes('__create');
  const hasReplaceHashWithCreate = /__create[\s\S]{0,400}replaceHash|replaceHash[\s\S]{0,400}__create/i.test(source);

  if (hasCreate && hasReplaceHashWithCreate) {
    addIssue(fails, 'H', COMPONENT_PATH, 'Forbidden __create hash reset/replaceHash logic detected');
  }

  const hasSkipRead = /__create[\s\S]{0,300}(skip|read|no\s*read)|selectedId\s*={2,3}\s*["']__create["']/i.test(source);
  if (!hasSkipRead) {
    addIssue(warnings, 'H', COMPONENT_PATH, 'No explicit __create skip-read rule detected');
  }
}


function withRuleInHint(issues) {
  return issues.map((issue) => ({
    ...issue,
    hint: `[${issue.rule}] ${issue.hint}`
  }));
}

function printIssues(prefix, issues) {
  for (const issue of issues) {
    const location = issue.line ? `${issue.file}:${issue.line}` : issue.file;
    console.log(`${prefix} ${location} :: ${issue.hint}`);
  }
}

function checkBackendFoldersGuard(fails) {
  const candidates = getChangedFiles();
  for (const file of candidates) {
    if (file.startsWith('backend/sap_backend/') || file.startsWith('sap_backend/')) {
      addIssue(fails, 'I', file, 'backend/sap_backend/ must remain untouched');
    }
  }
}

const fails = [];
const warnings = [];
const runtimeFiles = getRuntimeFiles();
const runtimeChecks = [
  checkForbiddenFallbackNetwork,
  checkForbiddenExpand,
  checkForbiddenFlexibleRouter
];
const staticChecks = [
  checkServiceRootContract,
  checkODataV2AndUseBatch,
  checkSearchViewContract,
  checkSegmentFiltersContract
];

runtimeChecks.forEach((run) => run(fails, runtimeFiles));
staticChecks.forEach((run) => run(fails));
checkCreateModeContract(fails, warnings);
if (process.env.ALLOW_BACKEND_CHANGES !== '1') checkBackendFoldersGuard(fails);

if (fails.length) {
  console.log('FAIL gateway-parity-validator');
  printIssues('- [FAIL]', withRuleInHint(fails));
  if (warnings.length) {
    printIssues('- [WARNING]', withRuleInHint(warnings));
  }
  process.exit(1);
}

if (warnings.length) {
  printIssues('WARNING', withRuleInHint(warnings));
}

console.log('PASS gateway-parity-validator');
process.exit(0);
