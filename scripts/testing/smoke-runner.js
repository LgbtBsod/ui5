const { runNetworkSmoke, runLockSmoke, runNavigationSmoke } = require('./smoke-runtime-surface');
const { runDeltaSmoke } = require('./smoke-delta');
const { runFeedbackSmoke } = require('./smoke-feedback');
const { runFrontendVariablesSmoke } = require('./smoke-frontend-variables');
const { createFileExistenceSmoke } = require('./smoke-file-check');

async function runProductSmokePacks() {
  const checks = [];
  checks.push(...(await runNetworkSmoke()));
  checks.push(...(await runDeltaSmoke()));
  checks.push(...(await runLockSmoke()));
  checks.push(...(await runNavigationSmoke()));
  checks.push(...(await runFeedbackSmoke()));
  checks.push(...(await runFrontendVariablesSmoke()));
  return checks;
}

async function runGovernanceSmokePacks() {
  return createFileExistenceSmoke(
    'governance',
    [
      'backend/sap_backend/EVIDENCE_ACCEPTANCE_MATRIX.md',
      'backend/sap_backend/OWNER_SIGNOFF_TRACKER.md',
      'docs/audit/ERROR_REMEDIATION_PLAN.md'
    ],
    'release/governance evidence artifact exists'
  );
}

async function runSmokePacks() {
  return runProductSmokePacks();
}

module.exports = {
  runGovernanceSmokePacks,
  runProductSmokePacks,
  runSmokePacks
};
