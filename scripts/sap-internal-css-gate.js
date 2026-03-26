#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();
const allowlistPath = path.join(ROOT, 'scripts', 'sap-internal-css-allowlist.json');
const allowlist = fs.existsSync(allowlistPath) ? JSON.parse(fs.readFileSync(allowlistPath, 'utf8')) : {};
const MAX_ALLOWLIST_FILES = 14;
const issues = [];

function walk(dir) {
  if (!fs.existsSync(dir)) {
    return;
  }
  fs.readdirSync(dir, { withFileTypes: true }).forEach((entry) => {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      walk(full);
      return;
    }
    if (!entry.name.endsWith('.css')) {
      return;
    }
    const relative = path.relative(ROOT, full).replace(/\\/g, '/');
    const lines = fs.readFileSync(full, 'utf8').split(/\r?\n/);
    let fileHasPrivateSelectors = false;
    lines.forEach((lineText, index) => {
      if (/\.(sapM|sapUi|sapF|sapMITB|sapMSwt|sapMDialog|sapUiComp)/.test(lineText)) {
        fileHasPrivateSelectors = true;
      }
      if (/\.(sapM|sapUi|sapF|sapMITB|sapMSwt|sapMDialog|sapUiComp)/.test(lineText) && !/allow-private-ui5-selector/.test(lineText) && !allowlist[relative]) {
        issues.push(`${relative}:${index + 1} private SAP selector in app-owned CSS`);
      }
    });
    if (allowlist[relative] && !fileHasPrivateSelectors) {
      issues.push(`${relative} has stale SAP selector allowlist entry`);
    }
  });
}

walk(path.join(ROOT, 'app', 'styles'));

Object.keys(allowlist).forEach((relative) => {
  const reason = String(allowlist[relative] || '').trim();
  if (!reason) {
    issues.push(`${relative} missing quarantine reason for private selector allowlist`);
    return;
  }
  if (reason.length < 24) {
    issues.push(`${relative} quarantine reason is too vague; document the remaining SAP dependency`);
  }
});

if (Object.keys(allowlist).length > MAX_ALLOWLIST_FILES) {
  issues.push(`private selector allowlist grew to ${Object.keys(allowlist).length} files; keep quarantine at or below ${MAX_ALLOWLIST_FILES}`);
}

if (issues.length) {
  console.log(['FAIL sap-internal-css-gate', ...issues.map((issue) => `- ${issue}`)].join('\n'));
  process.exit(1);
}

console.log('PASS sap-internal-css-gate');
