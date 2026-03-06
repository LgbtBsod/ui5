#!/usr/bin/env node
const fs = require('fs');

const file = 'Component.js';
const txt = fs.readFileSync(file, 'utf8');
const hasEdit = /WORKFLOW_EDIT_MODE/.test(txt) || /\"\/mode\"\) === \"EDIT\"/.test(txt);
const hasLock = /WORKFLOW_LOCK_STATUS/.test(txt) || /\"\/lockOperationState\"\) === \"LOCKED\"/.test(txt);
const hasDirty = /\"\/isDirty\"/.test(txt);

if (!hasEdit || !hasLock || !hasDirty) {
  console.error('autosave-lock-guard-gate failed: missing EDIT/LOCKED/dirty guard in autosave path');
  process.exit(1);
}
console.log('autosave-lock-guard-gate passed');
