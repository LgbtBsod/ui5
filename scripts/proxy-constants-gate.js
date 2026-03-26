#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();
const issues = [];

function walk(dir) {
  if (!fs.existsSync(dir)) {
    return [];
  }
  return fs.readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      return walk(full);
    }
    return /\.(js|xml)$/i.test(entry.name) ? [full] : [];
  });
}

function rel(file) {
  return path.relative(ROOT, file).replace(/\\/g, '/');
}

walk(path.join(ROOT, 'app')).forEach((file) => {
  const text = fs.readFileSync(file, 'utf8');
  const lines = text.split(/\r?\n/);
  lines.forEach((lineText, index) => {
    if (/\bDetailMessageKeyConstants\b|\bDetailMessageCodeConstants\b/.test(lineText)) {
      issues.push(`${rel(file)}:${index + 1} DetailContracts used as proxy message owner`);
    }
    if (/MessageKeyConstants\b/.test(lineText) && /\.CODES\b/.test(lineText)) {
      issues.push(`${rel(file)}:${index + 1} message keys file used as code owner`);
    }
    if (/var\s+\w*MESSAGE\w*\s*=\s*DetailContracts\b/.test(lineText)) {
      issues.push(`${rel(file)}:${index + 1} DetailContracts used as message-key proxy owner`);
    }
    if (/var\s+\w*MESSAGE\w*CODES?\w*\s*=\s*DetailContracts\b/.test(lineText)) {
      issues.push(`${rel(file)}:${index + 1} DetailContracts used as message-code proxy owner`);
    }
    if (/DetailContracts\.(ATTACHMENT_|CHECKLIST_|DELETE_|DETAIL_|INTEGRATION_|LOCK_|STATUS_|VALIDATION_)/.test(lineText)) {
      issues.push(`${rel(file)}:${index + 1} DetailContracts still carries frontend message/text ownership`);
    }
  });
});

if (issues.length) {
  console.log(['FAIL proxy-constants-gate', ...issues.map((issue) => `- ${issue}`)].join('\n'));
  process.exit(1);
}

console.log('PASS proxy-constants-gate');
