#!/usr/bin/env node
const fs = require('fs');
const { resolveFromRoot } = require('../qa-shared');
const src = fs.readFileSync(resolveFromRoot(process.cwd(), 'service/domain/detail/usecases/AutosaveDetailUseCase.js'),'utf8');
if (!/buildDeltaPayload/.test(src)) {
  console.error('FAIL: autosave must build delta payload when delta missing');
  process.exit(1);
}
if (/delta\s*:\s*\{\s*\}/.test(src) || /AUTOSAVE_EMPTY_DELTA/.test(src) === false) {
  console.error('FAIL: empty delta guard missing');
  process.exit(1);
}
console.log('PASS autosave-input-contract-gate');
