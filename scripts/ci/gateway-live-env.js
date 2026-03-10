#!/usr/bin/env node

const fs = require('fs');
const http = require('http');
const https = require('https');
const path = require('path');
const { spawn } = require('child_process');

const rootDir = path.resolve(__dirname, '..', '..');
const mockRoot = path.join(rootDir, 'backend', 'mock_gateway');
const runtimeDir = path.join(rootDir, 'docs', 'runtime');
const pythonBinRaw = process.env.PYTHON_BIN || process.env.PYTHON || 'python';
const pythonBin = path.basename(pythonBinRaw) === pythonBinRaw ? pythonBinRaw : path.resolve(rootDir, pythonBinRaw);
const backendPort = Number(process.env.GATEWAY_SMOKE_BACKEND_PORT || 8000);
const uiPort = Number(process.env.GATEWAY_SMOKE_UI_PORT || 8080);
const backendBase = process.env.GATEWAY_SMOKE_BACKEND_BASE || `http://127.0.0.1:${backendPort}`;
const backendUrl = `${backendBase}/sap/opu/odata/sap/Z_UI5_SRV/$metadata`;
const uiUrl = process.env.GATEWAY_SMOKE_UI_URL || `http://127.0.0.1:${uiPort}/index.html`;
const useExternalBackend = String(process.env.GATEWAY_SMOKE_EXTERNAL_BACKEND || '').trim() === '1';
const pidFiles = {
  backend: path.join(runtimeDir, 'gateway-live-backend.pid'),
  ui: path.join(runtimeDir, 'gateway-live-ui.pid')
};
const logFiles = {
  backend: path.join(runtimeDir, 'gateway-live-backend.log'),
  ui: path.join(runtimeDir, 'gateway-live-ui.log')
};

function ensureRuntimeDir() {
  fs.mkdirSync(runtimeDir, { recursive: true });
}

function readPid(file) {
  if (!fs.existsSync(file)) {
    return 0;
  }
  const value = String(fs.readFileSync(file, 'utf8') || '').trim();
  return Number(value || 0);
}

function writePid(file, pid) {
  fs.writeFileSync(file, `${pid}\n`, 'utf8');
}

function removeFile(file) {
  if (fs.existsSync(file)) {
    fs.unlinkSync(file);
  }
}

function isProcessAlive(pid) {
  if (!pid || !Number.isFinite(pid)) {
    return false;
  }
  try {
    process.kill(pid, 0);
    return true;
  } catch (_error) {
    return false;
  }
}

function stopPidFile(file) {
  const pid = readPid(file);
  if (!pid) {
    removeFile(file);
    return;
  }
  if (isProcessAlive(pid)) {
    try {
      process.kill(pid, 'SIGTERM');
    } catch (_error) {
    }
  }
  removeFile(file);
}

function requestOk(targetUrl) {
  const lib = targetUrl.startsWith('https:') ? https : http;
  return new Promise((resolve) => {
    const req = lib.get(targetUrl, (res) => {
      res.resume();
      resolve(res.statusCode >= 200 && res.statusCode < 500);
    });
    req.on('error', () => resolve(false));
    req.setTimeout(3000, () => {
      req.destroy();
      resolve(false);
    });
  });
}

async function waitForUrl(targetUrl, label, timeoutMs) {
  const startedAt = Date.now();
  while (Date.now() - startedAt < timeoutMs) {
    if (await requestOk(targetUrl)) {
      return;
    }
    await new Promise((resolve) => setTimeout(resolve, 400));
  }
  throw new Error(`${label} did not start on ${targetUrl} within ${timeoutMs}ms`);
}

function spawnDetached(name, command, args, options) {
  const out = fs.openSync(logFiles[name], 'a');
  return new Promise((resolve, reject) => {
    const child = spawn(command, args, {
      cwd: options.cwd,
      env: options.env,
      detached: true,
      shell: false,
      stdio: ['ignore', out, out],
      windowsHide: true
    });

    child.once('error', (error) => {
      reject(new Error(`${name} spawn failed: ${error.message}`));
    });
    child.once('spawn', () => {
      child.unref();
      writePid(pidFiles[name], child.pid);
      resolve(child.pid);
    });
  });
}

async function start() {
  ensureRuntimeDir();
  if (!useExternalBackend) {
    stopPidFile(pidFiles.backend);
  }
  stopPidFile(pidFiles.ui);

  if (!useExternalBackend) {
    await spawnDetached('backend', pythonBin, ['-m', 'uvicorn', 'main:app', '--host', '127.0.0.1', '--port', String(backendPort)], {
      cwd: mockRoot,
      env: { ...process.env }
    });
  } else {
    removeFile(pidFiles.backend);
  }
  await spawnDetached('ui', pythonBin, ['scripts/dev_static_server.py', String(uiPort)], {
    cwd: rootDir,
    env: { ...process.env, UI5_BACKEND_BASE: backendBase }
  });

  await waitForUrl(backendUrl, useExternalBackend ? 'SAP Gateway' : 'Mock backend', 30000);
  await waitForUrl(uiUrl, 'UI server', 30000);

  console.log(`Backend: ${backendUrl}${useExternalBackend ? ' (external)' : ''}`);
  console.log(`UI: ${uiUrl}`);
  console.log(`Logs: ${runtimeDir}`);
}

function stop() {
  stopPidFile(pidFiles.ui);
  if (!useExternalBackend) {
    stopPidFile(pidFiles.backend);
  } else {
    removeFile(pidFiles.backend);
  }
  console.log(`Stopped gateway live env. Logs: ${runtimeDir}`);
}

async function main() {
  const action = (process.argv[2] || 'start').toLowerCase();
  if (action === 'start') {
    await start();
    return;
  }
  if (action === 'stop') {
    stop();
    return;
  }
  throw new Error(`Unsupported action: ${action}`);
}

main().catch((error) => {
  console.error(error && error.message ? error.message : String(error));
  process.exit(1);
});
