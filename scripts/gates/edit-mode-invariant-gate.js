#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const file = path.resolve(__dirname, '../../service/domain/detail/usecases/EnterEditUseCase.js');
const txt = fs.readFileSync(file, 'utf8');
if (!txt.includes('WORKFLOW_DETAIL_EDIT_MODE, "EDIT"') || !txt.includes('WORKFLOW_DETAIL_LOCK_STATE, "LOCKED"')) {
  console.error('EDIT invariant not enforced in EnterEditUseCase');
  process.exit(1);
}
console.log('edit-mode-invariant-gate passed');
