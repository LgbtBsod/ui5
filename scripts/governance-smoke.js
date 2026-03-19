#!/usr/bin/env node

const { runGovernanceSmokePacks } = require('./testing/smoke-runner');

(async () => {
  const results = await runGovernanceSmokePacks();
  const failed = results.filter((item) => !item.ok);
  results.forEach((item) => console.log(`${item.ok ? 'PASS' : 'FAIL'} ${item.name} :: ${item.detail}`));
  if (failed.length) {
    process.exit(1);
  }
  console.log(`PASS governance-smoke (${results.length} checks)`);
})();
