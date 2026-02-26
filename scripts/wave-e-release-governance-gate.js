#!/usr/bin/env node
const fs = require('fs');

const requiredDocs = [
  'docs/adr/ADR-TEMPLATE.md',
  'docs/refactor-change-log.md',
  'docs/documentation-freeze-wave-e-v2.md',
  'docs/rollback-playbook-search-detail.md'
];

const missing = requiredDocs.filter((p) => !fs.existsSync(p));
if (missing.length > 0) {
  console.error(`Wave E governance gate failed: missing docs: ${missing.join(', ')}`);
  process.exit(1);
}

console.log('Wave E governance gate passed: ADR/governance/freeze/rollback docs are present.');
