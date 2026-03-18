#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();
const RULES = {
  '10_base.css': ['app', 'shell', 'page', 'chk', 'detail', 'search', 'analytics', 'toolbar', 'table', 'lock', 'access', 'workflow', 'skip', 'kbd', 'kpi'],
  '40_page_search.css': ['search', 'filter', 'result', 'table', 'smart', 'brand', 'data', 'input', 'toolbar', 'workflow', 'kpi', 'seg', 'max', 'backend', 'page', 'app', 'chk', 'analytics'],
  '41_page_detail.css': ['detail', 'attachment', 'check', 'barrier', 'lock', 'action', 'info', 'person', 'command', 'form', 'section', 'status', 'comment', 'table', 'toolbar', 'mode', 'ux', 'chip', 'app', 'chk', 'read', 'edit', 'flat', 'glass', 'location', 'pinned', 'is'],
  '42_page_analytics.css': ['analytics', 'chart', 'kpi', 'breakdown', 'year', 'comparison', 'builder', 'workflow', 'stage', 'rate', 'access', 'search', 'shell', 'app']
};
const IGNORE = new Set(['chkApp', 'chkAppRoot', 'appDark', 'appLight', 'sapUiBody']);

function stripComments(text) {
  return String(text || '').replace(/\/\*[\s\S]*?\*\//g, '');
}

function collectClasses(cssText) {
  const classes = new Set();
  const re = /\.([A-Za-z_][A-Za-z0-9_-]*)/g;
  let m;
  const stripped = stripComments(cssText);
  while ((m = re.exec(stripped)) !== null) {
    classes.add(m[1]);
  }
  return [...classes].sort();
}

function isConformant(className, prefixes) {
  if (IGNORE.has(className) || className.startsWith('sap') || className.startsWith('v-')) {
    return true;
  }
  const lower = className.toLowerCase();
  return prefixes.some((prefix) => lower.startsWith(prefix));
}

function main() {
  const issues = [];

  Object.entries(RULES).forEach(([fileName, prefixes]) => {
    const filePath = path.join(ROOT, 'app', 'css', 'modules', fileName);
    const text = fs.readFileSync(filePath, 'utf8');
    const classes = collectClasses(text);

    classes.forEach((className) => {
      if (isConformant(className, prefixes)) return;
      issues.push(`${fileName}: class namespace violation for .${className}`);
    });
  });

  if (issues.length) {
    console.error('css-namespace-governance-gate FAIL');
    issues.forEach((issue) => console.error(issue));
    process.exit(1);
  }

  console.log('css-namespace-governance-gate PASS (allowlist-free)');
}

main();
