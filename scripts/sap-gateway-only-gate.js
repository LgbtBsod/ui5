#!/usr/bin/env node

const qa = require('./qa-shared');
const { exitWithMappedIssues } = require('./lib/gate-result');
const runtimeGate = require('./lib/runtimeGateRuntime');

const ROOT = process.cwd();
const RUNTIME_DIRS = ['Component.js', 'controller', 'manager', 'service', 'infra', 'util', 'model'];

function detectGatewayTransportImports(files, violations) {
  files.forEach((file) => {
    const source = qa.readText(ROOT, file);
    qa.extractUi5Dependencies(source).forEach((dep) => {
      const sDep = String(dep.dep || '');
      if (sDep === 'sap/ui/thirdparty/jquery') {
        runtimeGate.pushPipeViolation(violations, file, qa.lineFromIndex(source, dep.index), 'jQuery transport import is forbidden in Gateway-only runtime');
      }
      if (sDep === 'checklist/app/service/backend/GatewayODataTransport') {
        runtimeGate.pushPipeViolation(violations, file, qa.lineFromIndex(source, dep.index), 'GatewayODataTransport import is forbidden in Gateway-only runtime');
      }
      if (sDep === 'checklist/app/service/backend/GatewayDirectHttp') {
        runtimeGate.pushPipeViolation(violations, file, qa.lineFromIndex(source, dep.index), 'GatewayDirectHttp is forbidden in Gateway-only runtime');
      }
      if (sDep === 'sap/ui/model/odata/v2/ODataModel' && file !== 'Component.js') {
        runtimeGate.pushPipeViolation(violations, file, qa.lineFromIndex(source, dep.index), 'ODataModel construction must stay centralized in Component.js');
      }
    });
  });
}

function detectGatewayOnlyStrings(files, violations) {
  const specs = [
    { regex: /\bGatewayDirectHttp\b/g, message: 'legacy direct HTTP transport is forbidden' },
    { regex: /\bdefaults_fallback\b/g, message: 'runtime defaults fallback is forbidden' },
    { regex: /\bruntime\.config\.fallback_used\b/g, message: 'runtime fallback telemetry is forbidden' },
    { regex: /\bgetWithFallback\s*\(/g, message: 'cache fallback API is forbidden on runtime path' },
    { regex: /\bX-Requested-With\b/g, message: 'manual X-Requested-With header is forbidden' },
    { regex: /\bjQuery\.ajax\s*\(/g, message: 'raw ajax is forbidden in Gateway-only runtime' }
  ];

  specs.forEach((spec) => {
    qa.scanRegexInFiles(ROOT, files, spec.regex, (file, source, match, line) => {
      runtimeGate.pushPipeViolation(violations, file, line || qa.lineFromIndex(source, match.index), spec.message);
    });
  });
}

function detectForbiddenGatewayPaths(files, violations) {
  const pattern = /\/(config\/frontend|FrontendRuntimeSettings|capabilities|persons\/suggest|hierarchy|actions\/|lock\/|ChecklistRoots|SearchRows|ChecklistChecksSet|ChecklistBarriersSet)\b/g;
  const targets = files.filter(function (file) {
    return /^(Component\.js|controller\/|infra\/|service\/backend\/)/.test(file) && file !== 'service/backend/GatewayClient.js';
  });
  qa.scanRegexInFiles(ROOT, targets, pattern, (file, source, match, line) => {
    runtimeGate.pushPipeViolation(violations, file, line || qa.lineFromIndex(source, match.index), `non-canonical Gateway path detected: /${match[1]}`);
  });
}

function detectManifestContract(violations) {
  const manifest = JSON.parse(qa.readText(ROOT, 'manifest.json'));
  const sapApp = (manifest && manifest['sap.app']) || {};
  const sapUi5 = (manifest && manifest['sap.ui5']) || {};
  const appDataSource = ((sapApp.dataSources || {}).mainService) || {};
  const ui5Model = ((sapUi5.models || {}).mainService) || {};
  const settings = ui5Model.settings || {};

  if (appDataSource.uri !== '/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/') {
    runtimeGate.pushPipeViolation(violations, 'manifest.json', null, 'sap.app mainService uri must be /sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/');
  }
  if (appDataSource.type !== 'OData') {
    runtimeGate.pushPipeViolation(violations, 'manifest.json', null, 'sap.app mainService type must be OData');
  }
  if (((appDataSource.settings || {}).odataVersion || '') !== '2.0') {
    runtimeGate.pushPipeViolation(violations, 'manifest.json', null, 'sap.app mainService odataVersion must be 2.0');
  }
  if (ui5Model.type !== 'sap.ui.model.odata.v2.ODataModel') {
    runtimeGate.pushPipeViolation(violations, 'manifest.json', null, 'sap.ui5 mainService model must be sap.ui.model.odata.v2.ODataModel');
  }
  if (ui5Model.dataSource !== 'mainService') {
    runtimeGate.pushPipeViolation(violations, 'manifest.json', null, 'sap.ui5 mainService model must reference sap.app dataSource "mainService"');
  }
  if (settings.useBatch !== true) {
    runtimeGate.pushPipeViolation(violations, 'manifest.json', null, 'mainService model must keep useBatch=true');
  }
  if (settings.tokenHandling !== true) {
    runtimeGate.pushPipeViolation(violations, 'manifest.json', null, 'mainService model must keep tokenHandling=true');
  }
  if (settings.defaultOperationMode !== 'Server') {
    runtimeGate.pushPipeViolation(violations, 'manifest.json', null, 'mainService model must keep defaultOperationMode="Server"');
  }
}

function detectComponentGatewayBootstrap(violations) {
  const source = [
    qa.readText(ROOT, 'Component.js'),
    qa.readText(ROOT, 'service/framework/ComponentInitRuntime.js')
  ].join('\n');
  if (!/new\s+ODataModel\s*\(\s*sMainServiceUri\s*,\s*\{[\s\S]*useBatch:\s*true[\s\S]*tokenHandling:\s*true/.test(source)) {
    runtimeGate.pushPipeViolation(violations, 'Component.js', null, 'Component bootstrap must construct ODataModel with useBatch=true and tokenHandling=true');
  }
  if (!/SettingsManager\.load\s*\(\s*GatewayBackendService\s*\)/.test(source)) {
    runtimeGate.pushPipeViolation(violations, 'Component.js', null, 'Component bootstrap must load RuntimeSettingsSet via GatewayBackendService');
  }
  if (!/frontendConfigSource"\s*,\s*"gateway_runtime"/.test(source)) {
    runtimeGate.pushPipeViolation(violations, 'Component.js', null, 'Component must mark runtime config source as gateway_runtime on success');
  }
  if (!/frontendConfigSource"\s*,\s*"gateway_runtime_error"/.test(source)) {
    runtimeGate.pushPipeViolation(violations, 'Component.js', null, 'Component must mark runtime config failures as gateway_runtime_error');
  }
}

(function main() {
  const violations = [];
  const runtimeFiles = runtimeGate.listRuntimeJsFiles(ROOT, RUNTIME_DIRS);
  const runtimeChecks = [detectGatewayTransportImports, detectGatewayOnlyStrings, detectForbiddenGatewayPaths];
  runtimeChecks.forEach((check) => check(runtimeFiles, violations));
  [detectManifestContract, detectComponentGatewayBootstrap].forEach((check) => check(violations));

  exitWithMappedIssues(
    'sap-gateway-only-gate',
    violations,
    runtimeGate.mapPipeIssue,
    { filesScanned: runtimeFiles.length },
    { asJson: process.argv.includes('--json') }
  );
})();
