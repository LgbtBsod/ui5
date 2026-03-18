#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { exitWithMappedIssues } = require('./lib/gate-result');
const { detectRuntimeRoot } = require('./qa-shared');

const runtimeRoot = detectRuntimeRoot(process.cwd());
const requiredVars = ['--semantic-danger', '--ui-sem-positive-label', '--ui-sem-critical-label'];
const cssParts = [
  'styles/modules/00_tokens.css',
  'styles/modules/01_theme-modes.css',
  'styles/modules/20_surface.css',
  'styles/modules/21_controls.css'
];

function read(relPath) {
  const full = path.join(process.cwd(), runtimeRoot, relPath);
  return fs.existsSync(full) ? fs.readFileSync(full, 'utf8') : '';
}

const styleCss = cssParts.map(read).join('\n');
const missingFiles = cssParts.filter((relPath) => !read(relPath));
const issues = [];

if (missingFiles.length) {
  issues.push(`Semantic contrast gate failed: missing style modules: ${missingFiles.join(', ')}`);
}
const missingVars = requiredVars.filter((value) => !styleCss.includes(value));
if (missingVars.length) {
  issues.push(`Semantic contrast gate failed: missing semantic vars: ${missingVars.join(', ')}`);
}
if (!styleCss.includes(':root.light-mode')) {
  issues.push('Semantic contrast gate failed: missing productive morning theme scope.');
}

exitWithMappedIssues(
  'semantic-contrast-gate',
  issues,
  (issue) => ({ file: `${runtimeRoot}/styles/modules/00_tokens.css`, message: issue }),
  { checks: requiredVars.length + 1 },
  { asJson: process.argv.includes('--json') }
);
