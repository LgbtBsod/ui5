#!/usr/bin/env node

const qa = require('./qa-shared');
const { exitWithColonIssues } = require('./lib/issueGateRuntime');

const root = process.cwd();
const files = qa.collectFilesByExtensions(root, ['service/domain'], ['.js'])
  .filter((file) => file.includes('/usecases/'));

const ALLOWED = new Set(['toast', 'busy', 'modelPatch', 'modelMerge', 'navigate', 'banner', 'dialog', 'log', 'inlineValidation', 'styleTokenEnable', 'styleTokenDisable']);
const violations = [];

for (const file of files) {
  const src = qa.readText(root, file);
  const hasEffects = /type\s*:\s*["'][^"']+["']/.test(src);

  if (hasEffects && !/\bResult\.(ok|fail)\s*\(/.test(src)) {
    violations.push(`${file}: missing Result.ok/fail usage`);
  }

  const typeRegex = /type\s*:\s*["']([^"']+)["']/g;
  let match = typeRegex.exec(src);
  while (match) {
    const effectType = match[1];
    if (!ALLOWED.has(effectType)) {
      violations.push(`${file}: forbidden effect type "${effectType}"`);
    }
    match = typeRegex.exec(src);
  }
}

exitWithColonIssues('effects-whitelist-gate', violations, { filesScanned: files.length, allowedEffects: ALLOWED.size }, { asJson: process.argv.includes('--json') });
