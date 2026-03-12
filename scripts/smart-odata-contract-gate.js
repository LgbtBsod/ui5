#!/usr/bin/env node

const { readText, extractUi5Dependencies, fileExists } = require('./qa-shared');
const { exitWithMappedIssues } = require('./lib/gate-result');

const ROOT = process.cwd();

function pushViolation(violations, message) {
  violations.push(`- ${message}`);
}

function hasAttr(block, name, value) {
  return new RegExp(`${name}="(?:${value})"`).test(block);
}

function hasAttrContaining(block, name, value) {
  return new RegExp(`${name}="[^"]*${value}[^"]*"`).test(block);
}

function collectControllerMethodSet() {
  const methods = new Set();
  const visited = new Set();
  const queue = ['controller/Search.controller.js'];
  while (queue.length) {
    const file = queue.shift();
    if (!file || visited.has(file)) continue;
    if (!fileExists(ROOT, file)) continue;
    visited.add(file);
    const src = readText(ROOT, file);
    [...src.matchAll(/([A-Za-z0-9_]+)\s*:\s*function\s*\(/g)].forEach((m) => methods.add(m[1]));
    extractUi5Dependencies(src).forEach(({ dep }) => {
      if (!dep.startsWith('PRODUCTION_CONTROL_CHECKLIST/controller/support/')) return;
      const depFile = dep.replace(/^PRODUCTION_CONTROL_CHECKLIST\//, '') + '.js';
      if (!visited.has(depFile)) queue.push(depFile);
    });
  }
  return methods;
}

function main() {
  const violations = [];
  const manifest = JSON.parse(readText(ROOT, 'manifest.json'));
  const sapUi5 = (manifest && manifest['sap.ui5']) || {};
  const searchView = readText(ROOT, 'view/Search.view.xml');
  const searchController = readText(ROOT, 'controller/Search.controller.js');

  const libs = (((sapUi5.dependencies || {}).libs) || {});
  if (!Object.prototype.hasOwnProperty.call(libs, 'sap.ui.comp')) {
    pushViolation(violations, 'manifest.json must declare sap.ui.comp for Smart controls');
  }

  if (!/xmlns:smartFilterBar="sap\.ui\.comp\.smartfilterbar"/.test(searchView)) {
    pushViolation(violations, 'Search.view.xml must declare SmartFilterBar namespace');
  }
  if (!/xmlns:smartTable="sap\.ui\.comp\.smarttable"/.test(searchView)) {
    pushViolation(violations, 'Search.view.xml must declare SmartTable namespace');
  }

  const smartFilterBlockMatch = searchView.match(/<smartFilterBar:SmartFilterBar\b([\s\S]*?)>\s*([\s\S]*?)<\/smartFilterBar:SmartFilterBar>/);
  const smartTableBlockMatch = searchView.match(/<smartTable:SmartTable\b([\s\S]*?)>\s*([\s\S]*?)<\/smartTable:SmartTable>/);
  const smartFilterBlock = smartFilterBlockMatch ? smartFilterBlockMatch[0] : '';
  const smartTableBlock = smartTableBlockMatch ? smartTableBlockMatch[0] : '';

  if (!smartFilterBlock) {
    pushViolation(violations, 'Search.view.xml must contain SmartFilterBar');
  } else {
    if (!hasAttr(smartFilterBlock, 'id', 'searchSmartFilterBar')) pushViolation(violations, 'SmartFilterBar id must be searchSmartFilterBar');
    if (!hasAttr(smartFilterBlock, 'entitySet', 'ChecklistSearchSet')) pushViolation(violations, 'SmartFilterBar must target ChecklistSearchSet');
    if (!hasAttr(smartFilterBlock, 'liveMode', 'false')) pushViolation(violations, 'SmartFilterBar must keep liveMode=false');
    if (!hasAttr(smartFilterBlock, 'showGoOnFB', 'true')) pushViolation(violations, 'SmartFilterBar must expose Go action');
    if (!hasAttr(smartFilterBlock, 'initialise', '\\.onSmartFilterInitialise')) pushViolation(violations, 'SmartFilterBar must bind initialise=.onSmartFilterInitialise');
    if (!hasAttr(smartFilterBlock, 'search', '\\.onSmartSearch')) pushViolation(violations, 'SmartFilterBar must bind search=.onSmartSearch');
    if (!hasAttr(smartFilterBlock, 'filterChange', '\\.onSmartFilterChanged')) pushViolation(violations, 'SmartFilterBar must bind filterChange=.onSmartFilterChanged');
  }

  if (!smartTableBlock) {
    pushViolation(violations, 'Search.view.xml must contain SmartTable');
  } else {
    if (!hasAttr(smartTableBlock, 'id', 'searchSmartTable')) pushViolation(violations, 'SmartTable id must be searchSmartTable');
    if (!hasAttr(smartTableBlock, 'entitySet', 'ChecklistSearchSet')) pushViolation(violations, 'SmartTable must target ChecklistSearchSet');
    if (!hasAttr(smartTableBlock, 'smartFilterId', 'searchSmartFilterBar')) pushViolation(violations, 'SmartTable must be linked to searchSmartFilterBar');
    if (!hasAttr(smartTableBlock, 'enableAutoBinding', 'false')) pushViolation(violations, 'SmartTable must keep enableAutoBinding=false');
    if (!hasAttr(smartTableBlock, 'beforeRebindTable', '\\.onBeforeSmartTableRebind')) pushViolation(violations, 'SmartTable must bind beforeRebindTable=.onBeforeSmartTableRebind');
    if (!hasAttr(smartTableBlock, 'initialise', '\\.onSmartTableInitialise')) pushViolation(violations, 'SmartTable must bind initialise=.onSmartTableInitialise');
    if (!hasAttr(smartTableBlock, 'useExportToExcel', 'false')) pushViolation(violations, 'SmartTable must keep useExportToExcel=false');
    if (!hasAttr(smartTableBlock, 'tableType', 'ResponsiveTable')) pushViolation(violations, 'SmartTable must keep ResponsiveTable tableType');
    if (!hasAttrContaining(smartTableBlock, 'requestAtLeastFields', 'Key')) pushViolation(violations, 'SmartTable requestAtLeastFields must include Key');
    if (/<(?:Table|t:Table)\b/.test(searchView)) pushViolation(violations, 'Search.view.xml must not define fallback local result tables outside SmartTable');
  }

  const availableMethods = collectControllerMethodSet();
  [
    'onSmartFilterInitialise',
    'onSmartFilterChanged',
    'onSmartTableInitialise',
    'onBeforeSmartTableRebind',
    'onSmartSearch'
  ].forEach((name) => {
    if (!availableMethods.has(name)) {
      pushViolation(violations, `Search.controller.js must define ${name}`);
    }
  });

  if (/sap\/ui\/model\/json\/JSONModel/.test(searchController)) {
    pushViolation(violations, 'Search.controller.js must not bootstrap local JSON dataset fallbacks');
  }
  if (/sap\/ui\/model\/odata\/v2\/ODataModel/.test(searchController)) {
    pushViolation(violations, 'Search.controller.js must not construct ODataModel directly');
  }
  if (/sap\/ui\/thirdparty\/jquery/.test(searchController)) {
    pushViolation(violations, 'Search.controller.js must not depend on jQuery transport');
  }
  if (/getModel\s*\(\s*["']mainService["']\s*\)\s*\.\s*(read|create|update|remove|callFunction|submitChanges)/.test(searchController)) {
    pushViolation(violations, 'Search.controller.js must stay facade-driven and not call mainService transport directly');
  }
  if (/rebindTable\s*\(/.test(searchController)) {
    pushViolation(violations, 'Search.controller.js must not force SmartTable.rebindTable() inside orchestration');
  }

  exitWithMappedIssues(
    'smart-odata-contract-gate',
    violations,
    function (line) { return { file: 'view/Search.view.xml', message: String(line) }; },
    { checks: violations.length },
    { asJson: process.argv.includes('--json') }
  );
}

main();
