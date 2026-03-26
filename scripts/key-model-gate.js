#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();
const metadataPath = path.join(ROOT, 'app', 'localService', 'metadata.xml');
const metadata = fs.readFileSync(metadataPath, 'utf8');
const mockApiPath = path.join(ROOT, 'backend', 'mock_gateway', 'api', 'gateway_canonical_api.py');
const mockApi = fs.readFileSync(mockApiPath, 'utf8');
const parentBoundaryBlock = (mockApi.match(/def _boundary_parent_key[\s\S]*?return ""/) || [''])[0];
const issues = [];

function add(message) {
  issues.push(message);
}

function entityBlock(name) {
  const match = metadata.match(new RegExp(`<EntityType Name="${name}"[\\s\\S]*?<\\/EntityType>`));
  return match ? match[0] : '';
}

['ChecklistRoot', 'ChecklistSearch', 'FunctionResult', 'ExportRow'].forEach((name) => {
  const block = entityBlock(name);
  if (!block) {
    add(`metadata missing entity block ${name}`);
    return;
  }
  if (/Name="PARENT_KEY"/.test(block)) {
    add(`root-facing entity ${name} exposes PARENT_KEY`);
  }
});

['ChecklistCheck', 'ChecklistBarrier', 'Attachment'].forEach((name) => {
  const block = entityBlock(name);
  if (!/Name="PARENT_KEY"/.test(block)) {
    add(`child entity ${name} missing PARENT_KEY`);
  }
  if (!/Name="DB_KEY"/.test(block)) {
    add(`child entity ${name} missing DB_KEY`);
  }
});

['FunctionResult', 'ExportRow'].forEach((name) => {
  const block = entityBlock(name);
  if (/RootKey|RootId/.test(block)) {
    add(`${name} still exposes legacy RootKey/RootId`);
  }
  if (!/Name="DB_KEY"/.test(block)) {
    add(`${name} missing canonical DB_KEY`);
  }
});

if (/body\.get\("DB_KEY"\)|body\.get\("db_key"\)/.test(parentBoundaryBlock)) {
  add('mock gateway still treats child DB_KEY as parent-key compatibility input');
}

if ((mockApi.match(/re\.sub\(r"\\bRootKey\\b\|\\bRootId\\b"/g) || []).length !== 1) {
  add('mock gateway still normalizes RootKey inline instead of via a single ingress helper');
}

const allowlist = new Set([
  'backend/mock_gateway/api/gateway_canonical_api.py',
  'backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap',
  'app/localService/metadata.xml'
]);

function walk(dir) {
  if (!fs.existsSync(dir)) {
    return [];
  }
  return fs.readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      return walk(full);
    }
    return [full];
  });
}

walk(path.join(ROOT, 'app')).concat(walk(path.join(ROOT, 'backend'))).forEach((file) => {
  const relative = path.relative(ROOT, file).replace(/\\/g, '/');
  if (
    allowlist.has(relative) ||
    relative.includes('__pycache__') ||
    relative.includes('/tests/') ||
    relative.includes('/test/') ||
    !/\.(js|xml|py|abap|md|txt)$/i.test(relative)
  ) {
    return;
  }
  const text = fs.readFileSync(file, 'utf8');
  const lines = text.split(/\r?\n/);
  lines.forEach((lineText, index) => {
    if (/^\s*(\/\*|\*|\/\/|#)/.test(lineText)) {
      return;
    }
    if (/\bRootKey\b|\bRootId\b/.test(lineText)) {
      issues.push(`${relative}:${index + 1} legacy RootKey/RootId leaked outside boundary`);
    }
  });
});

if (issues.length) {
  console.log(['FAIL key-model-gate', ...issues.map((issue) => `- ${issue}`)].join('\n'));
  process.exit(1);
}

console.log('PASS key-model-gate');
