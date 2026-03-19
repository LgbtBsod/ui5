#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const ROOT = process.cwd();

const SAP_SCRIPT_FILES = [
  'scripts/browser-smoke-detail-attachment-dirty-invariant.py',
  'scripts/browser-smoke-gateway-only-flow.py',
  'scripts/gateway-detail-lifecycle-proof.py',
  'scripts/manual-detail-exit-and-create-smoke.py',
  'scripts/ui-runtime-audit.py'
];

const DOC_FILES = [
  'backend/sap_backend/PRODUCTION_SMOKE_CHECKLIST.md',
  'backend/sap_backend/SAP_EVIDENCE_RUNBOOK.md',
  'backend/sap_backend/proof_records/EV-003_AUTHORIZATION_ALLOW_DENY.md',
  'backend/sap_backend/proof_records/EV-004_LOCK_LIFECYCLE.md',
  'backend/sap_backend/proof_records/EV-005_OPTIMISTIC_CONCURRENCY.md',
  'backend/sap_backend/proof_records/EV-006_FLP_LAUNCH.md',
  'backend/sap_backend/proof_records/EV-010_ACCESSIBILITY_KEYBOARD_FOCUS.md'
];

const FORBIDDEN = [
  'searchPaneHost',
  'detailPaneHost',
  'analyticsPaneHost',
  'data-ui5-app-ready'
];

const REQUIRED_LOCAL = ['PASS_LOCAL_BASELINE', 'BLOCKED_BACKEND', 'FAIL_UI_CONTRACT'];
const REQUIRED_SAP = ['PASS_SAP_EVIDENCE', 'BLOCKED_SAP_ENV', 'FAIL_PRODUCT_CONTRACT'];

function read(rel) {
  return fs.readFileSync(path.join(ROOT, rel), 'utf8');
}

const failures = [];

SAP_SCRIPT_FILES.forEach((file) => {
  const text = read(file);
  FORBIDDEN.forEach((needle) => {
    if (text.includes(needle)) {
      failures.push(`${file}: forbidden legacy smoke contract "${needle}"`);
    }
  });
});

const checklist = read('backend/sap_backend/PRODUCTION_SMOKE_CHECKLIST.md');
const runbook = read('backend/sap_backend/SAP_EVIDENCE_RUNBOOK.md');
const proofBundle = DOC_FILES.map((file) => ({ file, text: read(file) }));

REQUIRED_LOCAL.forEach((needle) => {
  if (!checklist.includes(needle) && !runbook.includes(needle)) {
    failures.push(`docs: local evidence result class missing: ${needle}`);
  }
});

REQUIRED_SAP.forEach((needle) => {
  const found = proofBundle.some((item) => item.text.includes(needle));
  if (!found) {
    failures.push(`docs: SAP evidence result class missing from runbook/proof records: ${needle}`);
  }
});

if (failures.length) {
  console.error('FAIL sap-evidence-contract-gate');
  failures.forEach((line) => console.error(` - ${line}`));
  process.exit(1);
}

console.log('PASS sap-evidence-contract-gate');
