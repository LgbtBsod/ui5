const { runNetworkSmoke, runLockSmoke, runNavigationSmoke } = require('./smoke-runtime-surface');
const { runDeltaSmoke } = require('./smoke-delta');
const { runFeedbackSmoke } = require('./smoke-feedback');

async function runSmokePacks() {
  const checks = [];
  checks.push(...(await runNetworkSmoke()));
  checks.push(...(await runDeltaSmoke()));
  checks.push(...(await runLockSmoke()));
  checks.push(...(await runNavigationSmoke()));
  checks.push(...(await runFeedbackSmoke()));
  return checks;
}

module.exports = { runSmokePacks };
