#!/usr/bin/env node

const {
  extractUi5Dependencies,
  lineFromIndex,
  readText,
} = require('./qa-shared');
const { exitWithMappedIssues } = require('./lib/gate-result');
const { listRuntimeJsFiles, pushPipeViolation, mapPipeIssue } = require('./lib/runtimeGateRuntime');

const ROOT = process.cwd();
const RUNTIME_DIRS = ['Component.js', 'controller', 'manager', 'service', 'util', 'model', 'infra'];

function detectForbiddenApis(files, violations) {
  const pattern = /\b(fetch\s*\(|new\s+XMLHttpRequest\s*\(|axios\b)/g;
  files.forEach((file) => {
    const source = readText(ROOT, file);
      let match = pattern.exec(source);
      while (match) {
      pushPipeViolation(violations, file, lineFromIndex(source, match.index), `forbidden API detected: ${match[1].trim()}`);
      match = pattern.exec(source);
    }
  });
}

function detectRestFallbackPaths(files, violations) {
  const pattern = /\/(config\/frontend|persons\/suggest|hierarchy)\b/g;
  files.forEach((file) => {
    const source = readText(ROOT, file);
      let match = pattern.exec(source);
      while (match) {
      pushPipeViolation(violations, file, lineFromIndex(source, match.index), `REST fallback path detected: /${match[1]}`);
      match = pattern.exec(source);
    }
  });
}

function detectGatewayFallbackLiterals(files, violations) {
  const pattern = /(GatewayDirectHttp|defaults_fallback|runtime\.config\.fallback_used|getWithFallback\s*\()/g;
  files.forEach((file) => {
    const source = readText(ROOT, file);
      let match = pattern.exec(source);
      while (match) {
      pushPipeViolation(violations, file, lineFromIndex(source, match.index), `gateway fallback contract violation: ${match[1]}`);
      match = pattern.exec(source);
    }
  });
}

function detectRealBackendImports(files, violations) {
  files.forEach((file) => {
    const source = readText(ROOT, file);
    extractUi5Dependencies(source).forEach((dep) => {
      if (String(dep.dep || '').includes('RealBackendService')) {
        pushPipeViolation(violations, file, lineFromIndex(source, dep.index), 'RealBackendService import is forbidden in runtime code');
      }
    });
  });
}

function detectLiteralIntervals(files, violations) {
  const pattern = /(setInterval|setTimeout)\s*\([\s\S]*?,\s*(\d+)\s*\)/g;
  files.forEach((file) => {
    const source = readText(ROOT, file);
    let match = pattern.exec(source);
    while (match) {
      const api = match[1];
      const delay = Number(match[2]);
      const line = lineFromIndex(source, match.index);
      const context = source.slice(Math.max(0, match.index - 120), Math.min(source.length, match.index + 180));
      const intervalSensitive = /poll|heartbeat|autosave|retry|refresh|idle|lock|gcd/i.test(context);
      const uiMicrodelay = api === 'setTimeout' && delay <= 50;
      if (intervalSensitive && !uiMicrodelay) {
        pushPipeViolation(violations, file, line, `${api} uses numeric literal delay ${delay}`);
      }
      match = pattern.exec(source);
    }
  });
}

function detectRuntimeSettingsLoad(violations) {
  const settingsSource = readText(ROOT, 'manager/SettingsManager.js');
  if (!/readEntity\(\s*["']RuntimeSettingsSet["']\s*,\s*["']Key='GLOBAL'["']/.test(settingsSource)) {
    pushPipeViolation(violations, 'manager/SettingsManager.js', null, "RuntimeSettingsSet('GLOBAL') load not detected");
  }

  const componentSource = [
    readText(ROOT, 'Component.js'),
    readText(ROOT, 'service/framework/ComponentInitRuntime.js')
  ].join('\n');
  if (!/SettingsManager\.load\(/.test(componentSource)) {
    pushPipeViolation(violations, 'Component.js', null, 'Component startup does not call SettingsManager.load(...)');
  }
}

function detectSanitizationBeforeApply(violations) {
  const component = readText(ROOT, 'Component.js');
  const applyPos = component.indexOf('_applyFrontendRuntimeConfig: function');
  if (applyPos < 0) {
    pushPipeViolation(violations, 'Component.js', null, '_applyFrontendRuntimeConfig was not found');
    return;
  }
  const slice = component.slice(applyPos, applyPos + 3500);
  if (!/RuntimeTimerSanitizer\.sanitizeTimers\(/.test(slice)) {
    pushPipeViolation(violations, 'Component.js', lineFromIndex(component, applyPos), 'sanitizeTimers missing before applying timers');
  }
}

(function main() {
  const violations = [];
  const files = listRuntimeJsFiles(ROOT, RUNTIME_DIRS);

  detectForbiddenApis(files, violations);
  detectRestFallbackPaths(files, violations);
  detectGatewayFallbackLiterals(files, violations);
  detectRealBackendImports(files, violations);
  detectLiteralIntervals(files, violations);
  detectRuntimeSettingsLoad(violations);
  detectSanitizationBeforeApply(violations);

  exitWithMappedIssues(
    'runtime-settings-gate',
    violations,
    mapPipeIssue,
    { filesScanned: files.length },
    { asJson: process.argv.includes('--json') }
  );
})();
