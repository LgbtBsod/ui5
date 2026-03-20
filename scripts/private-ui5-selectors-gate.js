#!/usr/bin/env node

const { runGate } = require('./lib/gateRunner');
const { scanPatterns } = require('./lib/patternScan');
const { readJsonSafe } = require('./lib/auditInput');
const path = require('path');

const PRIVATE_PATTERNS = [
  { id: 'private-sapUiComp-filterbar-toolbar', regex: /\.sapUiCompFilterBarToolbar/g, message: 'Private SmartFilterBar toolbar selector is forbidden outside allowlist' },
  { id: 'private-sapUiComp-smarttable-toolbar', regex: /\.sapUiCompSmartTableToolbar/g, message: 'Private SmartTable toolbar selector is forbidden outside allowlist' },
  { id: 'private-sapUxAP-objectpage', regex: /\.sapUxAPObjectPage[A-Za-z-]*/g, message: 'Private ObjectPage renderer selector is forbidden outside allowlist' },
  { id: 'private-sapFFCL-column', regex: /\.sapFFCLColumn[A-Za-z-]*/g, message: 'Private FlexibleColumnLayout selector is forbidden outside allowlist' },
  { id: 'private-sapUiTable-internals', regex: /\.sapUiTable(?:Cnt|Ctrl|CtrlScr|ColHdrCnt|ColCell|Cell|Tr)\b/g, message: 'Private sap.ui.table renderer selector is forbidden outside allowlist' },
  { id: 'private-sapMListTbl-internals', regex: /\.sapMListTbl(?:Cnt|HeaderCell|Cell|Row)\b/g, message: 'Private sap.m table renderer selector is forbidden outside allowlist' }
];

const ALLOWLIST_PATH = path.join(process.cwd(), 'scripts', 'private-ui5-selectors-allowlist.json');
const TEMP_ALLOWLIST = new Set(Object.keys(readJsonSafe(ALLOWLIST_PATH, {})));

runGate({
  name: 'private-ui5-selectors-gate',
  include: ['app/**/*.js', 'app/styles/**/*.css', 'app/test/**/*.js'],
  check: ({ file, text }) => {
    if (TEMP_ALLOWLIST.has(file)) {
      return [];
    }
    return scanPatterns(file, text, PRIVATE_PATTERNS);
  }
});
