#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();
const TARGET_DIRS = [
  path.join(ROOT, 'app', 'controller'),
  path.join(ROOT, 'app', 'service', 'domain'),
  path.join(ROOT, 'app', 'service', 'features')
];
const allowlistPath = path.join(ROOT, 'scripts', 'dom-hack-allowlist.json');
const allowlist = fs.existsSync(allowlistPath) ? JSON.parse(fs.readFileSync(allowlistPath, 'utf8')) : {};
const ALLOWLIST = new Set(Object.keys(allowlist));
const MAX_ALLOWLIST_FILES = 14;
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

Object.keys(allowlist).forEach((relative) => {
  const reason = String(allowlist[relative] || '').trim();
  if (!reason) {
    issues.push(`${relative} missing DOM quarantine reason`);
    return;
  }
  if (reason.length < 24) {
    issues.push(`${relative} DOM quarantine reason is too vague; document why public UI5 APIs are insufficient`);
  }
});

if (Object.keys(allowlist).length > MAX_ALLOWLIST_FILES) {
  issues.push(`DOM allowlist grew to ${Object.keys(allowlist).length} files; keep quarantine at or below ${MAX_ALLOWLIST_FILES}`);
}

if (issues.length) {
  console.log(['FAIL dom-hack-gate', ...issues.map((issue) => `- ${issue}`)].join('\n'));
  process.exit(1);
}

console.log('PASS dom-hack-gate');
