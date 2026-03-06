#!/usr/bin/env node
const { execSync } = require('child_process');

function runCmd(cmd) {
  try { return { ok: true, out: execSync(cmd, { encoding: 'utf8', stdio: 'pipe' }) }; }
  catch (e) { return { ok: false, out: (e.stdout || '') + (e.stderr || '') }; }
}

function checkConstitution() {
  const out = runCmd('npm run qa');
  const txt = out.out || '';
  const violations = [];
  if (!out.ok) {
    if (/layer-map|controller-import-whitelist-gate|architecture-gate/i.test(txt)) violations.push('Layer Integrity violated');
    if (/usecase-no-ui5-import-gate|domain/i.test(txt)) violations.push('Domain Isolation violated');
    if (/statepaths-schema-consistency-gate|lockOperationState/i.test(txt)) violations.push('State Authority violated');
    if (/autosave-input-contract-gate|edit-requires-lock-gate/i.test(txt)) violations.push('Autosave Safety violated');
    if (/network-contract-verifier|runtime-settings-gate/i.test(txt)) violations.push('Workflow Safety violated');
  }
  return {
    constitutionalOk: violations.length === 0,
    violations,
    evidence: out.ok ? 'qa-pass' : 'qa-fail'
  };
}

module.exports = { checkConstitution };
