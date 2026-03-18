#!/usr/bin/env node
const { readJsonSafe } = require('./lib/auditInput');

function fail(msg) {
  console.error(msg);
  process.exit(1);
}

function readResultsOrFail(reportPath, usageMessage, invalidMessage) {
  if (!reportPath) {
    fail(usageMessage);
  }

  const report = readJsonSafe(reportPath, null);
  if (!report) {
    fail(`Failed to read JSON report: ${reportPath}`);
  }

  const results = Array.isArray(report) ? report : (report && Array.isArray(report.results) ? report.results : null);
  if (!results) {
    fail(invalidMessage);
  }

  return results;
}

function ensureAllPassed(results, failureMessageBuilder) {
  const failed = results.filter((r) => r.status !== 'passed');
  if (failed.length > 0) {
    fail(failureMessageBuilder(failed.length));
  }
}

function ensureRequiredTests(results, requiredNames, messagePrefix) {
  const names = new Set(results.map((r) => r.name));
  const missing = requiredNames.filter((name) => !names.has(name));
  if (missing.length > 0) {
    fail(`${messagePrefix}: missing required tests: ${missing.join(', ')}`);
  }
}

const WAVE_REGRESSION_PROFILES = {
  waveC: {
    usageMessage: 'Usage: node scripts/wave-c-regression-gate.js /path/to/unit-smoke-report.json',
    invalidMessage: 'Report must be an array of test results or an object with results[]',
    failCountMessage: (failedCount) => `Wave C regression gate failed: ${failedCount} test(s) are not passed.`,
    missingPrefix: 'Wave C regression gate failed',
    requiredTests: [
      'WaveB3CriticalJourneysMatrix',
      'DetailSaveConflictUseCase',
      'DetailEditOrchestrationUseCase',
      'SearchRetryMessagePresentationUseCase',
      'SearchActionMessagePresentationUseCase',
      'OperationalKpiInstrumentationUseCase'
    ],
    passMessage: (count) => `Wave C regression gate passed: ${count} smoke test(s), required matrix present.`
  },
  waveD: {
    usageMessage: 'Usage: node scripts/wave-d-regression-gate.js /path/to/unit-smoke-report.json',
    invalidMessage: 'Report must be an array of test results or object with results[]',
    failCountMessage: (failedCount) => `Wave D regression gate failed: ${failedCount} non-passed test(s).`,
    missingPrefix: 'Wave D regression gate failed',
    requiredTests: [
      'OperationalKpiInstrumentationUseCase',
      'KpiSnapshotExportUseCase',
      'StartupCapabilityDiagnosticsUseCase',
      'ComponentStartupDiagnosticsOrchestrationUseCase',
      'SearchSelectionOpenFlowUseCase',
      'SearchStateSyncUseCase',
      'SearchExecuteFlowUseCase',
      'SearchCreateCopyFlowUseCase',
      'DetailCloseFlowOrchestrationUseCase',
      'DetailToggleEditOrchestrationUseCase',
      'DetailSaveFlowOrchestrationUseCase',
      'DetailSelectionMetaSyncUseCase',
      'SearchInlineAnalyticsRefreshOrchestrationUseCase'
    ],
    passMessage: (count) => `Wave D regression gate passed: ${count} smoke test(s), required maintainability/diagnostics checks present.`
  }
};

module.exports = {
  readResultsOrFail,
  ensureAllPassed,
  ensureRequiredTests,
  runWaveRegressionGate,
  WAVE_REGRESSION_PROFILES
};

function runWaveRegressionGate(config) {
  const results = readResultsOrFail(config.reportPath, config.usageMessage, config.invalidMessage);
  ensureAllPassed(results, (failedCount) => config.failCountMessage(failedCount));
  ensureRequiredTests(results, config.requiredTests, config.missingPrefix);
  console.log(config.passMessage(results.length));
}
