#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();
const allowlistPath = path.join(ROOT, 'scripts', 'sap-internal-css-allowlist.json');
const allowlist = fs.existsSync(allowlistPath) ? JSON.parse(fs.readFileSync(allowlistPath, 'utf8')) : {};
const cssFiles = [
  'app/styles/modules/controls/24_switches_and_toggles.css',
  'app/styles/modules/controls/26_controls.css',
  'app/styles/modules/controls/31_feedback_runtime.css',
  'app/styles/modules/controls/33_overflow_and_badges.css'
];
const issues = [];

cssFiles.forEach((relative) => {
  const file = path.join(ROOT, relative);
  if (!fs.existsSync(file)) {
    return;
  }
  const lines = fs.readFileSync(file, 'utf8').split(/\r?\n/);
  let fileHasPrivateSelectors = false;
  lines.forEach((lineText, index) => {
    if (/\.(sapM|sapUi|sapF|sapMITB)/.test(lineText)) {
      fileHasPrivateSelectors = true;
    }
    if (/\.(sapM|sapUi|sapF|sapMITB)/.test(lineText) && !/allow-private-ui5-selector/.test(lineText) && !allowlist[relative]) {
      issues.push(`${relative}:${index + 1} private SAP selector in app-owned CSS`);
    }
  });
  if (allowlist[relative] && !fileHasPrivateSelectors) {
    issues.push(`${relative} has stale SAP selector allowlist entry`);
  }
});

if (issues.length) {
  console.log(['FAIL sap-internal-css-gate', ...issues.map((issue) => `- ${issue}`)].join('\n'));
  process.exit(1);
}

console.log('PASS sap-internal-css-gate');
