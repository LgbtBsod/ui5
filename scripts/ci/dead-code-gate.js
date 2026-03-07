#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { scanFile } = require('../lib/js-deps-scanner');
const { listFiles } = require('../lib/fileWalker');
const { exitWithGateResult, maybeWriteSuggestedPatch } = require('../lib/gate-result');
const { createDeadCodeIssue } = require('./gate-issue-presets');
const { readJsonSafe } = require('../lib/auditInput');
const { detectRuntimeRoot } = require('../qa-shared');

const root = path.resolve(__dirname, '../..');
const runtimeRoot = detectRuntimeRoot(root);
const allowlistPath = path.join(root, 'scripts/ci/dead-code-allowlist.json');
const allow = (readJsonSafe(allowlistPath, { patterns: [] }).patterns) || [];


function globToRegExp(glob) {
  const escaped = glob.replace(/[.+^${}()|[\]\\]/g, '\\$&');
  return new RegExp('^' + escaped.replace(/\*/g, '.*') + '$');
}

function isAllowed(file) {
  return allow.some((a) => globToRegExp(a.pattern).test(file));
}

(function main() {
  const asJson = process.argv.includes('--json');
  const dirs = ['controller', 'service', 'infra', 'util', 'manager'];
  const files = dirs.flatMap((d) => listFiles(root, { include: [`${d}/*.js`, `${d}/**/*.js`] })).sort();
  const entryFiles = [path.join(runtimeRoot, 'Component.js').replace(/\\/g, '/')].filter((file) => fs.existsSync(path.join(root, file)));
  const scanFiles = [...new Set([...files, ...entryFiles])].sort();
  const rev = Object.fromEntries(files.map((f) => [f, []]));

  scanFiles.forEach((f) => scanFile(f, { rootDir: root }).forEach((d) => {
    if (d.resolved && rev[d.resolved]) rev[d.resolved].push(f);
  }));

  const dead = files.filter((f) => !isAllowed(f) && (!rev[f] || rev[f].length === 0));
  const errors = dead.map((file) => {
    const patchPath = maybeWriteSuggestedPatch('dead-code.unreferenced-module', {
      path: file,
      unifiedDiff: `# candidate removal patch (manual review required)\n# file: ${file}\n# verify reverse deps and runtime dynamic references before delete\n`
    });
    return createDeadCodeIssue(file, patchPath);
  });

  exitWithGateResult('dead-code-gate', errors, { filesScanned: files.length, allowlistPatterns: allow.length, deadCandidates: dead.length }, { asJson });
})();
