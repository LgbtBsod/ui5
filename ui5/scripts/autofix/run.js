#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const root = path.resolve(__dirname, '../..');
const outDir = path.join(__dirname, 'out');
const apply = process.argv.includes('--apply');
const safeApplyRules = new Set([]);
const { managerPurityRule, stylePurityRule } = require('./rules/purity-rules');
const rules = [
  require('./rules/feedback-import-rule'),
  stylePurityRule,
  managerPurityRule,
  require('./rules/scripts-helper-extract-rule')
];

if (!fs.existsSync(outDir)) fs.mkdirSync(outDir, { recursive: true });

rules.forEach((rule) => {
  const patches = rule.proposeFix(rule.detect(root));
  if (!patches.length) return;
  const patchFile = path.join(outDir, `${rule.id}.patch`);
  fs.writeFileSync(patchFile, patches.map((item) => item.patch).join('\n\n'));
  console.log(`patch generated: ${path.relative(root, patchFile)}`);
  if (apply && safeApplyRules.has(rule.id)) {
    console.log(`--apply enabled for safe rule ${rule.id} (no-op currently)`);
  }
});

console.log('autofix complete (patch-only by default)');
