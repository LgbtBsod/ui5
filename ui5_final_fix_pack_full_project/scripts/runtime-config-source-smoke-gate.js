#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

const root = process.cwd();

function read(relPath) {
  return fs.readFileSync(path.join(root, relPath), 'utf8');
}

function assertIncludes(source, needle, label) {
  if (!source.includes(needle)) {
    throw new Error(`Missing ${label}: ${needle}`);
  }
}

function assertCount(source, needle, expected, label) {
  const count = source.split(needle).length - 1;
  if (count !== expected) {
    throw new Error(`Unexpected count for ${label}. Expected ${expected}, got ${count}`);
  }
}

const constRef = 'FrontendConfigConstants.SOURCES.RUNTIME_SETTINGS_GLOBAL';

const settingsManager = read('app/service/runtime/SettingsManager.js');
assertCount(settingsManager, `source: ${constRef}`, 2, 'SettingsManager success/failure summary source');

const componentInit = read('app/service/framework/ComponentInitRuntime.js');
assertIncludes(componentInit, `source: ${constRef}`, 'runtime apply source payload');
assertIncludes(componentInit, `TelemetryRuntime.runtimeConfig(${constRef})`, 'runtime.config.loaded source');
assertIncludes(componentInit, `TelemetryRuntime.runtimeConfig(\n                        ${constRef},`, 'runtime.config.load_failed source');

const applyRuntime = read('app/service/domain/shared/usecases/ApplyRuntimeSettingsUseCase.js');
assertIncludes(applyRuntime, `oConfig.source || ${constRef}`, 'fallback source');

console.log('runtime-config-source-smoke-gate passed');
