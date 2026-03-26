#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();
const TARGETS = ['app', 'test', 'tests'];
const JS_EXTENSIONS = new Set(['.js']);
const ISSUES = [];
const FORBIDDEN_DETAIL_CONTRACT_MEMBERS = [
  'ATTACHMENT_DELETED',
  'ATTACHMENT_DRAFT_STAGE_HINT',
  'ATTACHMENT_UPLOADED',
  'CHECKLIST_DELETED',
  'DELETE_CHECKLIST_CONFIRM',
  'DETAIL_ACCESS_DENIED_MESSAGE',
  'DETAIL_ACCESS_DENIED_TITLE',
  'DETAIL_CREATE_PERMISSION_DENIED',
  'DETAIL_DELETE_PERMISSION_DENIED',
  'DETAIL_DRAFT_CHANGED',
  'DETAIL_DRAFT_CLEAN',
  'DETAIL_DRAFT_LOCAL',
  'DETAIL_EMPTY_ATTACHMENTS_SAVED_TEXT',
  'DETAIL_EMPTY_ATTACHMENTS_TEXT',
  'DETAIL_FORCED_READ_ONLY_TITLE',
  'DETAIL_VIEW_PERMISSION_DENIED',
  'INTEGRATION_EDIT_CONFIRM',
  'INTEGRATION_EDIT_CONFIRM_TITLE',
  'LOCK_ACQUIRE_FAILED',
  'LOCK_EXPIRED',
  'LOCK_EXPIRED_TAKEOVER_PROMPT',
  'LOCK_HEARTBEAT_FAILED',
  'LOCK_IDLE_TIMEOUT',
  'LOCK_KILLED',
  'LOCK_LOST',
  'LOCK_LOST_BANNER_TITLE',
  'LOCK_RELEASE_FAILED',
  'LOCK_STEAL_OWN_SESSION_PROMPT',
  'LOCK_STATUS_FAILED',
  'STATUS_CANCELLED',
  'STATUS_COMPLETED',
  'STATUS_DRAFT',
  'STATUS_FAILED',
  'STATUS_IN_PROCESS',
  'STATUS_OK',
  'REQUIRED_FIELD_HINT',
  'VALIDATION_SUMMARY_TITLE'
];

function walk(dir) {
  if (!fs.existsSync(dir)) {
    return [];
  }
  return fs.readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      return walk(full);
    }
    return JS_EXTENSIONS.has(path.extname(entry.name)) ? [full] : [];
  });
}

function rel(file) {
  return path.relative(ROOT, file).replace(/\\/g, '/');
}

function addIssue(file, line, message) {
  ISSUES.push(`${rel(file)}:${line} ${message}`);
}

TARGETS.flatMap((dir) => walk(path.join(ROOT, dir))).forEach((file) => {
  const text = fs.readFileSync(file, 'utf8');
  const lines = text.split(/\r?\n/);
  lines.forEach((lineText, index) => {
    const line = index + 1;
    if (/DetailContracts\.CODES\b/.test(lineText)) {
      addIssue(file, line, 'DetailContracts.CODES is forbidden; use MessageCodeConstants');
    }
    if (/\bDetailMessageKeyConstants\b|\bDetailMessageCodeConstants\b/.test(lineText)) {
      addIssue(file, line, 'legacy detail message proxy alias is forbidden');
    }
    if (/var\s+\w*MESSAGE\w*\s*=\s*DetailContracts\b/.test(lineText)) {
      addIssue(file, line, 'DetailContracts cannot own frontend message keys; use MessageKeyConstants');
    }
    if (/var\s+\w*MESSAGE\w*CODES?\w*\s*=\s*DetailContracts\b/.test(lineText)) {
      addIssue(file, line, 'DetailContracts cannot own machine-readable message codes; use MessageCodeConstants');
    }
    FORBIDDEN_DETAIL_CONTRACT_MEMBERS.forEach((member) => {
      if (lineText.includes(`DetailContracts.${member}`)) {
        addIssue(file, line, `DetailContracts.${member} is forbidden; move consumer to MessageKeyConstants`);
      }
    });
  });
});

if (ISSUES.length) {
  console.log(['FAIL detail-contracts-owner-gate', ...ISSUES.map((issue) => `- ${issue}`)].join('\n'));
  process.exit(1);
}

console.log('PASS detail-contracts-owner-gate');
