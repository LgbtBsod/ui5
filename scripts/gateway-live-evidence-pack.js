#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const http = require('http');
const https = require('https');
const { spawnSync } = require('child_process');
const { readJsonSafe } = require('./lib/auditInput');

const ROOT = path.resolve(__dirname, '..');
const ARTIFACT_DIR = path.join(ROOT, 'docs', 'artifacts');
const SUMMARY_JSON = path.join(ARTIFACT_DIR, 'gateway-live-evidence-summary.json');
const SUMMARY_MD = path.join(ARTIFACT_DIR, 'gateway-live-evidence-summary.md');
const SERVICE_PATH = '/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV';
const PYTHON_BIN = process.env.PYTHON_BIN || process.env.PYTHON || 'python';
const UI_URL = process.env.GATEWAY_SMOKE_UI_URL || 'http://127.0.0.1:8080/index.html';
const SERVICE_URL = (
  process.env.GATEWAY_SMOKE_SERVICE_URL
  || `${String(process.env.GATEWAY_SMOKE_BACKEND_BASE || '').replace(/\/$/, '')}${SERVICE_PATH}`
).replace(/\/$/, '');

const RESULT_PASS = 'PASS_SAP_EVIDENCE';
const RESULT_BLOCKED = 'BLOCKED_SAP_ENV';
const RESULT_FAIL = 'FAIL_PRODUCT_CONTRACT';

function nowIso() {
  return new Date().toISOString();
}

function ensureArtifactsDir() {
  fs.mkdirSync(ARTIFACT_DIR, { recursive: true });
}

function isLocalService(url) {
  return /127\.0\.0\.1|localhost/i.test(String(url || ''));
}

function printUsage() {
  console.log('Usage: npm run smoke:gateway:live-evidence');
  console.log('Required env for live run:');
  console.log('  GATEWAY_SMOKE_EXTERNAL_BACKEND=1');
  console.log('  GATEWAY_SMOKE_BACKEND_BASE=https://qa-host');
  console.log('Optional env:');
  console.log('  GATEWAY_SMOKE_UI_URL=http://127.0.0.1:8080/index.html');
  console.log(`  GATEWAY_SMOKE_SERVICE_URL=https://qa-host${SERVICE_PATH}`);
}

function requestUrl(targetUrl, options) {
  const cfg = options || {};
  const method = String(cfg.method || 'GET').toUpperCase();
  const headers = { ...(cfg.headers || {}) };
  const lib = String(targetUrl).startsWith('https:') ? https : http;
  return new Promise((resolve) => {
    const req = lib.request(targetUrl, { method, headers }, (res) => {
      const chunks = [];
      res.on('data', (chunk) => chunks.push(Buffer.from(chunk)));
      res.on('end', () => {
        const body = Buffer.concat(chunks).toString('utf8');
        resolve({
          ok: res.statusCode >= 200 && res.statusCode < 400,
          status: res.statusCode || 0,
          headers: res.headers || {},
          body: body.slice(0, 1500),
          url: targetUrl
        });
      });
    });
    req.on('error', (error) => {
      resolve({
        ok: false,
        status: 0,
        headers: {},
        body: '',
        url: targetUrl,
        error: error && error.message ? error.message : String(error)
      });
    });
    req.setTimeout(15000, () => {
      req.destroy(new Error('timeout'));
    });
    req.end();
  });
}

function runStep(command, args, name) {
  const result = spawnSync(command, args, {
    cwd: ROOT,
    encoding: 'utf8',
    stdio: 'pipe',
    shell: false,
    env: { ...process.env }
  });
  return {
    name,
    command,
    args,
    exitCode: Number.isInteger(result.status) ? result.status : 1,
    ok: (result.status || 0) === 0,
    stdout: String(result.stdout || '').trim(),
    stderr: String(result.stderr || '').trim()
  };
}

function existingJson(filePath) {
  return fs.existsSync(filePath) ? readJsonSafe(filePath, null) : null;
}

function hasEnvIndicators(value) {
  const text = JSON.stringify(value || '').toLowerCase();
  return [
    '401',
    '403',
    '502',
    '503',
    'forbidden',
    'unauthorized',
    'csrf',
    'sso',
    'lrep',
    'metadata',
    'blocked_by_environment',
    'environment'
  ].some((needle) => text.includes(needle));
}

function collectBlockingCategories(payloads) {
  const categories = new Set();
  const text = JSON.stringify(payloads || []).toLowerCase();
  if (text.includes('401') || text.includes('403') || text.includes('unauthorized') || text.includes('forbidden') || text.includes('sso')) {
    categories.add('auth');
    categories.add('roles');
  }
  if (text.includes('metadata')) categories.add('metadata');
  if (text.includes('csrf')) categories.add('csrf');
  if (text.includes('lrep') || text.includes('flex')) categories.add('flex/LREP');
  if (text.includes('lock')) categories.add('locks');
  if (text.includes('autosave')) categories.add('autosave');
  if (text.includes('savechanges') || text.includes('save')) categories.add('save flow');
  if (text.includes('attachment')) categories.add('attachments');
  return Array.from(categories);
}

function classifyOutcome(preflight, steps, reports) {
  const payloads = [preflight, steps, reports];
  const preflightFailed = (preflight || []).some((item) => !item.ok);
  if (preflightFailed) {
    return RESULT_BLOCKED;
  }
  const failedSteps = (steps || []).filter((item) => !item.ok);
  if (!failedSteps.length) {
    return RESULT_PASS;
  }
  const envBlocked = failedSteps.some((item) => hasEnvIndicators(item))
    || hasEnvIndicators(reports);
  return envBlocked ? RESULT_BLOCKED : RESULT_FAIL;
}

function summarizeEvidence(resultClass, preflight, steps, reports, serviceUrl) {
  const executedLiveSteps = steps.some((item) => item.name === 'live-env.start' && item.ok);
  const files = [
    'docs/artifacts/gateway-only-smoke-report.json',
    'docs/artifacts/gateway-browser-smoke-report.json',
    'docs/artifacts/detail-lifecycle-proof/report.json',
    'docs/artifacts/gateway-lock-multisession-replay.json',
    'docs/artifacts/gateway-live-evidence-summary.json'
  ].filter((file) => fs.existsSync(path.join(ROOT, file)) && (executedLiveSteps || file.endsWith('gateway-live-evidence-summary.json')));
  return {
    generatedAt: nowIso(),
    environmentUsed: serviceUrl,
    authMode: 'Browser SSO',
    executionMode: 'external QA Gateway via local UI server',
    resultClass,
    executedScripts: steps.map((item) => ({
      name: item.name,
      exitCode: item.exitCode,
      ok: item.ok
    })),
    preflight,
    producedArtifacts: files,
    reports: executedLiveSteps ? reports : {},
    blockingCategories: collectBlockingCategories([preflight, steps, executedLiveSteps ? reports : {}]),
    provenOnLiveQa: executedLiveSteps ? [
      'metadata/runtime bootstrap',
      'search/detail/analytics browser flow',
      'create/save/autosave',
      'lock acquire/heartbeat/release',
      'attachment add/get/delete',
      'repeated route enter/leave snapshots',
      'multisession lock replay'
    ] : [],
    notProvenDueToEnvironment: resultClass === RESULT_BLOCKED ? ['See blockingCategories and failed steps.'] : [],
    probableCodeDefects: resultClass === RESULT_FAIL ? ['See failed steps and report failures.'] : []
  };
}

function markdownSummary(summary) {
  const lines = [
    '# Gateway Live Evidence Summary',
    '',
    `- Generated at: ${summary.generatedAt}`,
    `- Environment: ${summary.environmentUsed}`,
    `- Auth mode: ${summary.authMode}`,
    `- Result class: ${summary.resultClass}`,
    '',
    '## Executed Scripts',
    ''
  ];
  summary.executedScripts.forEach((item) => {
    lines.push(`- ${item.name}: ${item.ok ? 'PASS' : 'FAIL'} (exit ${item.exitCode})`);
  });
  lines.push('', '## Blocking Categories', '');
  if (!summary.blockingCategories.length) {
    lines.push('- none');
  } else {
    summary.blockingCategories.forEach((item) => lines.push(`- ${item}`));
  }
  lines.push('', '## Produced Artifacts', '');
  summary.producedArtifacts.forEach((item) => lines.push(`- ${item.replace(/\\/g, '/')}`));
  return lines.join('\n') + '\n';
}

async function main() {
  if (process.argv.includes('--help')) {
    printUsage();
    return 0;
  }

  ensureArtifactsDir();
  const preflight = [];
  const steps = [];
  let summary;

  if (String(process.env.GATEWAY_SMOKE_EXTERNAL_BACKEND || '').trim() !== '1') {
    preflight.push({
      name: 'externalBackendFlag',
      ok: false,
      status: 0,
      url: '',
      body: '',
      error: 'set GATEWAY_SMOKE_EXTERNAL_BACKEND=1 for live QA evidence'
    });
  }
  if (!SERVICE_URL || SERVICE_URL === SERVICE_PATH || isLocalService(SERVICE_URL)) {
    preflight.push({
      name: 'serviceUrl',
      ok: false,
      status: 0,
      url: SERVICE_URL,
      body: '',
      error: 'live service URL is missing or still points to localhost'
    });
  }

  if (!preflight.some((item) => item.name === 'serviceUrl')) {
    const metadataUrl = `${SERVICE_URL}/$metadata`;
    preflight.push({ name: 'metadata', ...(await requestUrl(metadataUrl)) });
    preflight.push({ name: 'serviceRootHead', ...(await requestUrl(`${SERVICE_URL}/`, { method: 'HEAD' })) });
    const csrf = await requestUrl(`${SERVICE_URL}/`, { headers: { 'X-CSRF-Token': 'Fetch' } });
    preflight.push({
      name: 'csrfFetch',
      ...csrf,
      tokenPresent: Boolean(csrf.headers && (csrf.headers['x-csrf-token'] || csrf.headers['X-CSRF-Token']))
    });
  }

  try {
    if (!preflight.some((item) => !item.ok)) {
      steps.push(runStep(process.execPath, ['scripts/ci/gateway-live-env.js', 'start'], 'live-env.start'));
      if (!steps[steps.length - 1].ok) {
        throw new Error('live-env.start failed');
      }

      steps.push(runStep(process.execPath, ['scripts/gateway-live-smoke-runner.js'], 'gateway-live-smoke-runner'));
      const smokeReport = existingJson(path.join(ROOT, 'docs/artifacts/gateway-only-smoke-report.json')) || {};
      const createdRootId = String(smokeReport.createdRootId || '').trim();

      steps.push(runStep(PYTHON_BIN, ['scripts/gateway-detail-lifecycle-proof.py', UI_URL, SERVICE_URL], 'gateway-detail-lifecycle-proof'));
      steps.push(runStep(PYTHON_BIN, ['scripts/browser-smoke-gateway-only-flow.py', UI_URL, createdRootId], 'browser-smoke-gateway-only-flow'));
      if (createdRootId) {
        steps.push(runStep(PYTHON_BIN, ['scripts/gateway-lock-multisession-replay.py', SERVICE_URL, createdRootId], 'gateway-lock-multisession-replay'));
      } else {
        steps.push({
          name: 'gateway-lock-multisession-replay',
          command: PYTHON_BIN,
          args: ['scripts/gateway-lock-multisession-replay.py', SERVICE_URL, '<missing-root-id>'],
          exitCode: 1,
          ok: false,
          stdout: '',
          stderr: 'missing createdRootId from gateway-only-smoke-report.json'
        });
      }

      const scenarioInputsPresent = [
        'docs/artifacts/scenario-suite-report.json',
        'docs/artifacts/qa-crawl-report.json',
        'docs/artifacts/manual-p1p2-browser-evidence.json'
      ].every((file) => fs.existsSync(path.join(ROOT, file)));
      if (scenarioInputsPresent) {
        steps.push(runStep(PYTHON_BIN, ['scripts/scenario-evidence-index.py'], 'scenario-evidence-index'));
      }
    }
  } catch (error) {
    steps.push({
      name: 'live-pack.exception',
      command: '',
      args: [],
      exitCode: 1,
      ok: false,
      stdout: '',
      stderr: error && error.message ? error.message : String(error)
    });
  } finally {
    if (steps.some((item) => item.name === 'live-env.start' && item.ok)) {
      steps.push(runStep(process.execPath, ['scripts/ci/gateway-live-env.js', 'stop'], 'live-env.stop'));
    }
  }

  const reports = {
    gatewayOnlySmoke: existingJson(path.join(ROOT, 'docs/artifacts/gateway-only-smoke-report.json')),
    gatewayBrowserSmoke: existingJson(path.join(ROOT, 'docs/artifacts/gateway-browser-smoke-report.json')),
    gatewayDetailLifecycle: existingJson(path.join(ROOT, 'docs/artifacts/detail-lifecycle-proof/report.json')),
    gatewayLockReplay: existingJson(path.join(ROOT, 'docs/artifacts/gateway-lock-multisession-replay.json'))
  };
  const resultClass = classifyOutcome(preflight, steps, reports);
  summary = summarizeEvidence(resultClass, preflight, steps, reports, SERVICE_URL);

  fs.writeFileSync(SUMMARY_JSON, JSON.stringify(summary, null, 2) + '\n', 'utf8');
  fs.writeFileSync(SUMMARY_MD, markdownSummary(summary), 'utf8');
  console.log(JSON.stringify(summary, null, 2));
  return resultClass === RESULT_PASS ? 0 : 1;
}

main().then((code) => process.exit(code)).catch((error) => {
  console.error(error && error.stack ? error.stack : String(error));
  process.exit(1);
});
