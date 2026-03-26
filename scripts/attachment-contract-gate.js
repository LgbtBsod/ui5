#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();
const metadata = fs.readFileSync(path.join(ROOT, 'app', 'localService', 'metadata.xml'), 'utf8');
const mockApi = fs.readFileSync(path.join(ROOT, 'backend', 'mock_gateway', 'api', 'gateway_canonical_api.py'), 'utf8');
const appFiles = [];
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
    if (/\.(js|xml|py|md|txt)$/i.test(entry.name)) {
      appFiles.push(full);
    }
  });
}

function rel(file) {
  return path.relative(ROOT, file).replace(/\\/g, '/');
}

const attachmentBlock = (metadata.match(/<EntityType Name="Attachment"[\s\S]*?<\/EntityType>/) || [''])[0];
const attachmentFieldNames = [...attachmentBlock.matchAll(/Property Name="([^"]+)"/g)].map((match) => match[1]);
['DownloadUrl', 'DocumentHandle', 'PARENT_KEY', 'DB_KEY'].forEach((field) => {
  if (!new RegExp(`Name="${field}"`).test(attachmentBlock)) {
    issues.push(`metadata attachment missing ${field}`);
  }
});
if (/Name="Value"/.test(attachmentBlock)) {
  issues.push('metadata attachment still exposes Value');
}

if (!/DownloadUrl/.test(mockApi) || !/DocumentHandle/.test(mockApi)) {
  issues.push('mock attachment API missing DownloadUrl/DocumentHandle');
}
attachmentFieldNames.forEach((field) => {
  if (!new RegExp(`"${field}"\\s*:`).test(mockApi) && !['Description'].includes(field)) {
    issues.push(`mock attachment serialization missing metadata field ${field}`);
  }
});
if (/_boundary_parent_key\(item\)\s+or\s+root_hex/.test(mockApi) === false) {
  issues.push('mock attachment payload normalization no longer anchors child parent key to canonical PARENT_KEY/root fallback');
}

walk(path.join(ROOT, 'app'));
walk(path.join(ROOT, 'backend'));

appFiles.forEach((file) => {
  const relative = rel(file);
  if (relative.includes('__pycache__')) {
    return;
  }
  const text = fs.readFileSync(file, 'utf8');
  const lines = text.split(/\r?\n/);
  lines.forEach((lineText, index) => {
    if (
      /\bValue\b/.test(lineText) &&
      /Attachment/.test(lineText) &&
      !/ContentBase64|_fileBase64|Value"\)|Value", "value"|ContentBase64", "content_base64"/.test(lineText)
    ) {
      issues.push(`${relative}:${index + 1} attachment Value leaked into active path`);
    }
  });
});

appFiles.forEach((file) => {
  const relative = rel(file);
  if (relative.includes('__pycache__')) {
    return;
  }
  const text = fs.readFileSync(file, 'utf8');
  if (/ContentBase64/.test(text) && !/app\/infra\/adapters\/shared\/AttachmentRepoRuntime\.js$/.test(relative) && !/backend\/mock_gateway\//.test(relative)) {
    issues.push(`${relative} uses ContentBase64 outside the gateway attachment upload boundary`);
  }
});

if (issues.length) {
  console.log(['FAIL attachment-contract-gate', ...issues.map((issue) => `- ${issue}`)].join('\n'));
  process.exit(1);
}

console.log('PASS attachment-contract-gate');
