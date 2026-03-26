#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { detectRuntimeRoot, readText } = require('../qa-shared');
const { exitWithMappedIssues } = require('../lib/gate-result');

const ROOT = process.cwd();
const RUNTIME_ROOT = detectRuntimeRoot(ROOT);
const FRAMEWORK_DIR = path.join(ROOT, RUNTIME_ROOT, 'service', 'framework');

const TOKEN_RULES = [
  {
    label: 'boot-token',
    literals: ['boot_failed', 'bootstrap_app_failed', 'bootstrap_init_bundle_failed', 'load_current_user_failed', 'load_runtime_settings_failed'],
    allowedFiles: new Set([
      'app/service/framework/ComponentBootContracts.js',
      'app/service/runtime/component/ComponentLifecycleRuntime.js'
    ])
  },
  {
    label: 'listener-token',
    literals: ['workflow.mode.changed', 'lock.state.changed', 'pcct:fullSave', 'LOCK_OWNED', 'LOCK_RELEASED', 'You have unsaved changes'],
    allowedFiles: new Set([
      'app/service/framework/ComponentListenerContracts.js',
      'app/service/framework/ComponentAppRuntime.js',
      'app/service/runtime/component/ComponentLockEventsRuntime.js'
    ])
  },
  {
    label: 'save-guard-token',
    literals: ['networkUnavailable', 'objectSaveFailed', 'retryNowButton', 'sessionExpiredBanner', 'workingMessageLong', 'detail.save.guarded.failed', 'detail.save.guarded.success'],
    allowedFiles: new Set([
      'app/service/framework/ComponentSaveGuardContracts.js'
    ])
  },
  {
    label: 'feedback-token',
    literals: ['conflictDialogText', 'loadErrorMessage'],
    allowedFiles: new Set([
      'app/service/framework/EffectFeedbackContracts.js'
    ])
  }
];

function collectFrameworkFiles() {
  if (!fs.existsSync(FRAMEWORK_DIR)) {
    return [];
  }
  return fs.readdirSync(FRAMEWORK_DIR)
    .filter((name) => name.endsWith('.js'))
    .map((name) => path.join(FRAMEWORK_DIR, name))
    .sort();
}

function findLine(source, token) {
  const index = source.search(new RegExp(`["']${escapeRegExp(token)}["']`));
  if (index < 0) {
    return null;
  }
  return source.slice(0, index).split(/\r?\n/).length;
}

function escapeRegExp(value) {
  return String(value || '').replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function main() {
  const issues = [];
  const files = collectFrameworkFiles();

  files.forEach((absPath) => {
    const relPath = path.relative(ROOT, absPath).replace(/\\/g, '/');
    const source = readText(ROOT, relPath);

    TOKEN_RULES.forEach((rule) => {
      if (rule.allowedFiles.has(relPath)) {
        return;
      }
      rule.literals.forEach((token) => {
        const pattern = new RegExp(`["']${escapeRegExp(token)}["']`);
        if (pattern.test(source)) {
          issues.push({
            file: relPath,
            line: findLine(source, token),
            message: `${rule.label} must come from canonical contract, found literal "${token}"`
          });
        }
      });
    });
  });

  exitWithMappedIssues(
    'framework-token-drift-gate',
    issues,
    (item) => item,
    { scannedFiles: files.length, rules: TOKEN_RULES.length },
    { asJson: process.argv.includes('--json') }
  );
}

main();
