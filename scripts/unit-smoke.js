#!/usr/bin/env node

const { runSmokePacks } = require('./test-pack/smoke-runner');

(async () => {
  const results = await runSmokePacks();
  const failed = results.filter((item) => !item.ok);
  results.forEach((item) => console.log(`${item.ok ? 'PASS' : 'FAIL'} ${item.name} :: ${item.detail}`));
  if (failed.length) {
    process.exit(1);
  }
  console.log(`PASS unit-smoke (${results.length} checks)`);
})();
