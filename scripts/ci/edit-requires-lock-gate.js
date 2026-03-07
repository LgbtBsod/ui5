#!/usr/bin/env node
const fs = require('fs');
const { resolveFromRoot } = require('../qa-shared');
const src = fs.readFileSync(resolveFromRoot(process.cwd(), 'service/domain/detail/usecases/EnterEditUseCase.js'),'utf8');
if (/lockOk\)\s*\?\s*["']LOCKED/.test(src) && /WORKFLOW_DETAIL_EDIT_MODE[\s\S]*true/.test(src)) {
  console.error('FAIL: edit mode can be enabled without explicit lock guard');
  process.exit(1);
}
if (!/LOCK_ACQUIRE_FAILED/.test(src)) {
  console.error('FAIL: lock failure branch missing');
  process.exit(1);
}
console.log('PASS edit-requires-lock-gate');
