#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();
const TARGET_DIRS = [
  path.join(ROOT, 'app', 'controller'),
  path.join(ROOT, 'app', 'service', 'domain'),
  path.join(ROOT, 'app', 'service', 'features')
];
const ALLOWLIST = new Set([
  'app/ui5-background-runtime.js',
  'app/ui5-bootstrap-runtime.js',
  'app/service/framework/ThemeDomRuntime.js',
  'app/service/features/shell/runtime/AppShellDomRuntime.js',
  'app/service/framework/SemanticDomRuntime.js',
  'app/controller/detail/AttachmentDropZoneRuntime.js',
  'app/controller/detail/DetailControllerRuntime.js',
  'app/controller/detail/DetailInfoCardFactory.js',
  'app/service/features/detail/runtime/DetailRowBehaviorRuntime.js',
  'app/service/features/search/runtime/SearchReturnRediscoveryRuntime.js',
  'app/service/features/search/runtime/SearchSelectionRuntime.js',
  'app/service/features/search/runtime/SearchViewportRuntime.js',
  'app/service/features/shell/runtime/ShellLayoutRuntime.js',
  'app/service/features/shell/runtime/ShellViewportRuntime.js'
]);
const PATTERNS = [
  { regex: /\bquerySelector(All)?\s*\(/g, label: 'querySelector' },
  { regex: /\bclosest\s*\(/g, label: 'closest' },
  { regex: /\.classList\b/g, label: 'classList' },
  { regex: /\bgetDomRef\s*\(/g, label: 'getDomRef' },
  { regex: /\bResizeObserver\b/g, label: 'ResizeObserver' }
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

TARGET_DIRS.forEach(walk);

if (issues.length) {
  console.log(['FAIL dom-hack-gate', ...issues.map((issue) => `- ${issue}`)].join('\n'));
  process.exit(1);
}

console.log('PASS dom-hack-gate');
