#!/usr/bin/env node

const path = require('path');
const { spawnSync } = require('child_process');

const rootDir = path.resolve(__dirname, '..');
const pythonBin = process.env.PYTHON_BIN || process.env.PYTHON || 'python';
const uiUrl = process.env.GATEWAY_SMOKE_UI_URL || 'http://127.0.0.1:8080/index.html';
const serviceUrl = process.env.GATEWAY_SMOKE_SERVICE_URL || 'http://127.0.0.1:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV';

function run(command, args) {
  return spawnSync(command, args, {
    cwd: rootDir,
    stdio: 'inherit',
    shell: false
  });
}

let result = run(pythonBin, ['scripts/gateway-only-smoke-pack.py', uiUrl, serviceUrl]);
if ((result.status || 0) !== 0) {
  process.exit(result.status || 1);
}

result = run(process.execPath, ['scripts/gateway-only-smoke-gate.js']);
if ((result.status || 0) !== 0) {
  process.exit(result.status || 1);
}

result = run(process.execPath, ['scripts/gateway-smoke-invariant-gate.js']);
process.exit(result.status || 0);
