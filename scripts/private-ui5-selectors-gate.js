#!/usr/bin/env node

const { runGate } = require('./lib/gateRunner');
const { scanPatterns } = require('./lib/patternScan');

const PRIVATE_PATTERNS = [
  { id: 'private-sapUiComp-filterbar-toolbar', regex: /\.sapUiCompFilterBarToolbar/g, message: 'Private SmartFilterBar toolbar selector is forbidden outside allowlist' },
  { id: 'private-sapUiComp-smarttable-toolbar', regex: /\.sapUiCompSmartTableToolbar/g, message: 'Private SmartTable toolbar selector is forbidden outside allowlist' },
  { id: 'private-sapUxAP-objectpage', regex: /\.sapUxAPObjectPage[A-Za-z-]*/g, message: 'Private ObjectPage renderer selector is forbidden outside allowlist' },
  { id: 'private-sapFFCL-column', regex: /\.sapFFCLColumn[A-Za-z-]*/g, message: 'Private FlexibleColumnLayout selector is forbidden outside allowlist' },
  { id: 'private-sapUiTable-internals', regex: /\.sapUiTable(?:Cnt|Ctrl|CtrlScr|ColHdrCnt|ColCell|Cell|Tr)\b/g, message: 'Private sap.ui.table renderer selector is forbidden outside allowlist' },
  { id: 'private-sapMListTbl-internals', regex: /\.sapMListTbl(?:Cnt|HeaderCell|Cell|Row)\b/g, message: 'Private sap.m table renderer selector is forbidden outside allowlist' }
];

const TEMP_ALLOWLIST = new Set([
  'app/service/features/search/runtime/SearchSelectionFocusRuntime.js',
  'app/styles/modules/10_base.css',
  'app/styles/modules/22_skeleton.css',
  'app/styles/modules/controls/25_table_actions.css',
  'app/styles/modules/92_ui5_surface_tuning.css',
  'app/styles/modules/analytics/44_analytics_panels.css',
  'app/styles/modules/detail/44_detail_attachments.css',
  'app/styles/modules/dialogs/24_dialog_tables.css',
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
