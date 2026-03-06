const fs = require('fs');
const path = require('path');

function createGateResult(name, errors = [], stats = {}) {
  return { name, ok: errors.length === 0, errors, stats };
}

function toPrettyText(result) {
  if (result.ok) return `${result.name} PASS`;
  return `${result.name} FAIL\n` + result.errors.map((e) => `${e.file}${e.line ? ':' + e.line : ''} ${e.message}`).join('\n');
}

function maybeWriteSuggestedPatch(ruleId, suggestedPatch) {
  if (!suggestedPatch || !suggestedPatch.unifiedDiff) return null;
  const outDir = path.resolve(__dirname, '../autofix/out');
  fs.mkdirSync(outDir, { recursive: true });
  const safe = String(ruleId || 'rule').replace(/[^a-zA-Z0-9._-]/g, '_');
  const patchFile = path.join(outDir, `${safe}.patch`);
  fs.writeFileSync(patchFile, suggestedPatch.unifiedDiff, 'utf8');
  return path.relative(path.resolve(__dirname, '..'), patchFile).split(path.sep).join('/');
}

function finalizeAndExit(result, opts = {}) {
  const asJson = !!opts.asJson;
  if (asJson) console.log(JSON.stringify(result, null, 2));
  else console.log(toPrettyText(result));
  process.exit(result.ok ? 0 : 1);
}

function exitWithGateResult(name, errors, stats, opts) {
  finalizeAndExit(createGateResult(name, errors, stats), opts || {});
}

function exitWithMappedIssues(name, items, mapIssue, stats, opts) {
  const list = Array.isArray(items) ? items : [];
  const mapper = typeof mapIssue === 'function' ? mapIssue : function (item) { return item; };
  exitWithGateResult(name, list.map(mapper), stats, opts);
}

module.exports = { createGateResult, maybeWriteSuggestedPatch, finalizeAndExit, exitWithGateResult, exitWithMappedIssues };
