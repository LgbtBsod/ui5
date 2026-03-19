#!/usr/bin/env node

const { runGate } = require('./lib/gateRunner');
const { scanPatterns } = require('./lib/patternScan');

const PRIVATE_PATTERNS = [
  { id: 'private-sapUiComp-filterbar-toolbar', regex: /\.sapUiCompFilterBarToolbar/g, message: 'Private SmartFilterBar toolbar selector is forbidden outside allowlist' },
  { id: 'private-sapUiComp-smarttable-toolbar', regex: /\.sapUiCompSmartTableToolbar/g, message: 'Private SmartTable toolbar selector is forbidden outside allowlist' },
  { id: 'private-sapUxAP-objectpage', regex: /\.sapUxAPObjectPage[A-Za-z-]*/g, message: 'Private ObjectPage renderer selector is forbidden outside allowlist' },
  { id: 'private-sapFFCL-column', regex: /\.sapFFCLColumn[A-Za-z-]*/g, message: 'Private FlexibleColumnLayout selector is forbidden outside allowlist' }
];

const TEMP_ALLOWLIST = new Set([
  'app/styles/modules/detail/43_detail_tables.css',
  'app/styles/modules/dialogs/25_dialog_runtime_skin.css',
  'app/styles/modules/search/43_search_results_table.css'
]);

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
