#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();
const APP_DIR = path.join(ROOT, 'app');
const ALLOWLIST = new Set([
  'app/ui5-background-runtime.js',
  'app/ui5-bootstrap-runtime.js',
  'app/service/framework/ThemeDomRuntime.js',
  'app/service/features/shell/runtime/AppShellDomRuntime.js',
  'app/service/framework/SemanticDomRuntime.js'
]);
const PATTERNS = [
  { regex: /\bquerySelector(All)?\s*\(/g, label: 'querySelector' },
  { regex: /\bclosest\s*\(/g, label: 'closest' },
  { regex: /\.classList\b/g, label: 'classList' },
  { regex: /\bgetDomRef\s*\(/g, label: 'getDomRef' },
  { regex: /\bResizeObserver\b/g, label: 'ResizeObserver' },
  { regex: /\bdocument\./g, label: 'document' },
  { regex: /\bwindow\./g, label: 'window' }
];
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
    if (!entry.name.endsWith('.js')) {
      return;
    }
    const relative = path.relative(ROOT, full).replace(/\\/g, '/');
    if (ALLOWLIST.has(relative)) {
      return;
    }
    const lines = fs.readFileSync(full, 'utf8').split(/\r?\n/);
    lines.forEach((line, index) => {
      PATTERNS.forEach((pattern) => {
        if (pattern.regex.test(line)) {
          issues.push(`${relative}:${index + 1} DOM-dependent ${pattern.label} usage outside allowlist`);
        }
        pattern.regex.lastIndex = 0;
      });
    });
  });
}

walk(APP_DIR);

if (issues.length) {
  console.log(['FAIL dom-hack-gate', ...issues.map((issue) => `- ${issue}`)].join('\n'));
  process.exit(1);
}

console.log('PASS dom-hack-gate');
