#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { detectRuntimeRoot } = require('./qa-shared');
const { exitWithMappedIssues } = require('./lib/gate-result');
const runtimeRoot = detectRuntimeRoot(process.cwd());

function read(file) {
  const resolved = path.join(process.cwd(), runtimeRoot, file);
  return fs.existsSync(resolved) ? fs.readFileSync(resolved, 'utf8') : '';
}

const styleCss = [
  read('styles/app-styles.css'),
  read('styles/modules/01_theme-modes.css'),
  read('styles/modules/20_surface.css'),
  read('styles/modules/21_controls.css'),
  read('styles/modules/23_dialogs.css'),
  read('styles/modules/90_ui5_patches.css'),
  read('styles/modules/controls/24_switches_and_toggles.css'),
  read('styles/modules/dialogs/23_dialog_shell.css')
].join('\n');
const themeMixin = read('controller/base/ThemeMixin.js');
const themeService = read('service/framework/ThemeService.js');
const themePhilosophy = read('service/framework/ThemePhilosophy.js');
const indexHtml = read('index.html');
const bootstrapRuntime = read('ui5-bootstrap-runtime.js');

const issues = [];
function fail(message) { issues.push(message); }

if (!/data-sap-ui-theme="sap_fiori_3"/.test(indexHtml)
  && !/setAttribute\(\s*["']data-sap-ui-theme["']\s*,\s*["']sap_fiori_3["']\s*\)/.test(bootstrapRuntime)
  && !/DEFAULT_THEME\s*=\s*["']sap_fiori_3["']/.test(bootstrapRuntime)) {
  fail('UI5 bootstrap runtime must set sap_fiori_3 as the base theme.');
}
if (/sap_horizon/.test(bootstrapRuntime) || /sap_horizon/.test(indexHtml)) {
  fail('UI5 bootstrap runtime must not request Horizon themes on the UI5 1.71 path.');
}
if (!/sap_fiori_3/.test(themeService) || !/sap_fiori_3/.test(themePhilosophy)) {
  fail('Theme layer must normalize onto sap_fiori_3.');
}
if (/sap_fiori_3_dark/.test(themeMixin) && !/return "morning"/.test(themeMixin)) {
  fail('Theme override layer must guard dark-mode requests on the UI5 1.71 path.');
}
if (!/:root\.light-mode/.test(styleCss) || !/body\.appDark/.test(styleCss)) {
  fail('App style layer must expose both Morning/Night token modes.');
}
if (!/platformPrecisionEnterprise/.test(styleCss) || !/platformCalmModern/.test(styleCss) || !/platformPrecisionEnterprise/.test(themePhilosophy) || !/platformCalmModern/.test(themePhilosophy)) {
  fail('Theme philosophy bridge is missing distinct platform contracts.');
}
if (!/\.sapMSwt\b|\.sapMSwtCont\b|\.sapMSwtInner\b/.test(styleCss) || !/\.sapMInputBaseContentWrapper\b/.test(styleCss) || !/\.sapMDialog\b|\.sapMDialogScrollCont\b/.test(styleCss)) {
  fail('Core control styling coverage missing for switch/input/dialog.');
}
if (!/backdrop-filter\s*:/.test(styleCss)) {
  fail('Glass language missing: backdrop-filter not found in app style layer.');
}

if (issues.length) {
  exitWithMappedIssues(
    'design-language-gate',
    issues,
    (issue) => ({ file: 'styles/app-styles.css', message: issue }),
    { checks: issues.length },
    { asJson: process.argv.includes('--json') }
  );
}

exitWithMappedIssues('design-language-gate', [], function (issue) { return issue; }, { checks: 0 }, { asJson: process.argv.includes('--json') });
