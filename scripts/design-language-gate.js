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
  read('css/app-styles.css'),
  read('css/modules/01_theme-modes.css'),
  read('css/modules/20_surface.css'),
  read('css/modules/21_controls.css'),
  read('css/modules/23_dialogs.css'),
  read('css/modules/90_ui5_patches.css')
].join('\n');
const themeMixin = read('controller/base/ThemeMixin.js');
const themeService = read('util/ThemeService.js');
const themePhilosophy = read('util/ThemePhilosophy.js');
const indexHtml = read('index.html');
const bootstrapRuntime = read('ui5-bootstrap-runtime.js');

const issues = [];
function fail(message) { issues.push(message); }

if (!/data-sap-ui-theme="sap_horizon"/.test(indexHtml) && !/setAttribute\(\s*["']data-sap-ui-theme["']\s*,\s*["']sap_horizon["']\s*\)/.test(bootstrapRuntime)) {
  fail('UI5 bootstrap runtime must set sap_horizon as the base theme.');
}
if (!/sap_horizon_dark/.test(themeService) || !/sap_horizon_dark/.test(themePhilosophy)) {
  fail('Theme layer must support sap_horizon_dark.');
}
if (!/:root\.light-mode/.test(styleCss) || !/body\.appDark/.test(styleCss)) {
  fail('App style layer must expose both Morning/Night token modes.');
}
if (!/platformPrecisionEnterprise/.test(styleCss) || !/platformCalmModern/.test(styleCss) || !/platformPrecisionEnterprise/.test(themePhilosophy) || !/platformCalmModern/.test(themePhilosophy)) {
  fail('Theme philosophy bridge is missing distinct platform contracts.');
}
if (!/\.sapMSwt\b/.test(styleCss) || !/\.sapMInputBaseContentWrapper\b/.test(styleCss) || !/\.sapMDialog\b/.test(styleCss)) {
  fail('Core control styling coverage missing for switch/input/dialog.');
}
if (!/backdrop-filter\s*:/.test(styleCss)) {
  fail('Glass language missing: backdrop-filter not found in app style layer.');
}

if (issues.length) {
  exitWithMappedIssues(
    'design-language-gate',
    issues,
    (issue) => ({ file: 'css/app-styles.css', message: issue }),
    { checks: issues.length },
    { asJson: process.argv.includes('--json') }
  );
}

exitWithMappedIssues('design-language-gate', [], function (issue) { return issue; }, { checks: 0 }, { asJson: process.argv.includes('--json') });
