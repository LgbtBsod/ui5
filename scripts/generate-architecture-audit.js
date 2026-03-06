#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { scanFile } = require('./lib/js-deps-scanner');
const { listFiles } = require('./lib/fileWalker');
const { readJsonSafe, readTextSafe } = require('./lib/auditInput');

const root = path.resolve(__dirname, '..');
const DOCS = path.join(root, 'docs');
const runtimeDirs = ['controller', 'service', 'infra', 'util', 'manager'];
const shadowDirs = ['service/autosave', 'service/search', 'service/detail'];
const criticalNameRe = /(Facade|UseCase|Adapter|Manager|Coordinator|Loader|Policy|Builder|Mapper)\.js$/;

const deadCodeAllowlistPath = path.join(root, 'scripts/ci/dead-code-allowlist.json');

function wildcardToRegExp(pattern) {
  const escaped = pattern.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  const regex = '^' + escaped.replace(/\\\*/g, '.*') + '$';
  return new RegExp(regex);
}

function loadDeadAllowlistMatchers() {
  if (!fs.existsSync(deadCodeAllowlistPath)) return [];
  const parsed = readJsonSafe(deadCodeAllowlistPath, null);
  if (!parsed || typeof parsed !== 'object') {
    return [];
  }
  const items = Array.isArray(parsed.patterns) ? parsed.patterns : [];
  return items.map((i) => ({ pattern: i.pattern, reason: i.reason || 'allowlisted', re: wildcardToRegExp(i.pattern) }));
}

function matchAllowlist(file, matchers) {
  return matchers.find((m) => m.re.test(file)) || null;
}

function toRepoPath(p) { return path.relative(root, p).split(path.sep).join('/'); }
function read(file) { return readTextSafe(file, ''); }
function writeDoc(name, data) { fs.writeFileSync(path.join(DOCS, name), data); }


function listRuntimeFiles() { return runtimeDirs.flatMap((d) => listFiles(root, { include: [`${d}/*.js`, `${d}/**/*.js`] })).sort(); }

function featureSeed(file) {
  if (/^controller\/Search/.test(file) || /^service\/domain\/search\//.test(file)) return 'search';
  if (/^controller\/Detail/.test(file) || /^service\/domain\/detail\//.test(file)) return 'detail';
  if (/^service\/domain\/lock\//.test(file)) return 'lock';
  if (/^service\/domain\/autosave\//.test(file)) return 'autosave';
  if (/^service\/domain\/attachments\//.test(file)) return 'attachments';
  if (/^service\/domain\/dictionary\//.test(file)) return 'dictionary';
  if (/^service\/domain\/person\//.test(file)) return 'person';
  if (/^service\/domain\/shared\//.test(file)) return 'shared';
  if (/^service\/backend\//.test(file)) return 'backend';
  if (/^service\/framework\//.test(file)) return 'framework';
  if (/^infra\//.test(file)) return 'infra';
  return 'unknown';
}

function classifyUtil(file, graph, rev) {
  const deps = (graph[file] || []).map((d) => d.dep || '');
  const importers = rev[file] || [];
  const hasUi = deps.some((d) => d.startsWith('sap/ui/'));
  const hasBackend = deps.some((d) => d.includes('service/backend') || d.includes('infra/odata'));
  if ((importers.length === 0) && !hasUi && !hasBackend) return ['D', 'dead candidate (verify dynamic references)'];
  if (!hasUi && !hasBackend) return ['A', 'pure helper candidate for framework/shared'];
  if (hasBackend && !hasUi) return ['B', 'domain/helper touching backend, consider domain shared'];
  return ['C', 'UI helper/formatter'];
}

function findStaleRefs() {
  const targets = [
    ...listFiles(root, { include: ['scripts/**'] }),
    ...listFiles(root, { include: ['test/**'] }),
    ...listFiles(root, { include: ['tests/**'] })
  ].filter((file) => !file.startsWith('scripts/legacy/'));
  return [...new Set(targets)].filter((f) => /service\/usecase|sap_ui5\/service\/usecase/.test(read(path.join(root, f))));
}

function extractValidators() {
  const configPath = path.join(root, 'scripts/lib/qa-pipeline-config.js');
  if (fs.existsSync(configPath)) {
    try {
      const config = require(configPath);
      if (Array.isArray(config.validators)) {
        return config.validators
          .filter((item) => item && item.name && item.file)
          .map((item) => ({ name: String(item.name), file: String(item.file) }));
      }
    } catch (e) {
      // Fall through to regex parser for compatibility.
    }
  }

  const qaAll = path.join(root, 'scripts/qa-all.js');
  if (!fs.existsSync(qaAll)) return [];
  const txt = readTextSafe(qaAll, '');
  const out = [];
  const re = /\{\s*name:\s*'([^']+)'\s*,\s*file:\s*'([^']+)'\s*\}/g;
  let m = re.exec(txt);
  while (m) { out.push({ name: m[1], file: m[2] }); m = re.exec(txt); }
  return out;
}

function findCriticalDuplicates(files) {
  const byBase = {};
  files.forEach((f) => {
    if (!criticalNameRe.test(f)) return;
    const base = path.basename(f);
    (byBase[base] ||= []).push(f);
  });
  return Object.entries(byBase).filter(([, list]) => list.length > 1).map(([name, list]) => ({ name, files: list.sort() }));
}

(function main() {
  fs.mkdirSync(DOCS, { recursive: true });
  const files = listRuntimeFiles();
  const entryFiles = ['Component.js'].filter((file) => fs.existsSync(path.join(root, file)));
  const scanFiles = [...new Set([...files, ...entryFiles])].sort();
  const graph = Object.fromEntries(files.map((f) => [f, []]));
  const rev = Object.fromEntries(files.map((f) => [f, []]));

  scanFiles.forEach((f) => {
    const deps = scanFile(f, { rootDir: root });
    if (graph[f]) {
      graph[f] = deps;
    }
    deps.forEach((d) => { if (d.resolved && rev[d.resolved]) rev[d.resolved].push(f); });
  });
  Object.keys(rev).forEach((k) => { rev[k] = [...new Set(rev[k])].sort(); });

  writeDoc('dependency-graph.json', JSON.stringify(graph, null, 2));
  writeDoc('reverse-deps.json', JSON.stringify(rev, null, 2));

  const fmap = {};
  files.forEach((f) => {
    const seed = featureSeed(f);
    if (seed !== 'unknown') return (fmap[f] = { feature: seed, confidence: 1 });
    const features = [...new Set((rev[f] || []).map(featureSeed).filter((x) => x !== 'unknown'))];
    fmap[f] = (features.length === 1)
      ? { feature: features[0], confidence: 0.7 }
      : (features.length > 1)
        ? { feature: 'shared', confidence: 0.5, note: 'shared-candidate' }
        : { feature: 'unknown', confidence: 0.2 };
  });
  writeDoc('feature-map.json', JSON.stringify(fmap, null, 2));

  const byFeature = {};
  Object.entries(fmap).forEach(([f, v]) => { (byFeature[v.feature] ||= []).push(f); });
  const fmapMd = ['# Feature map', ''];
  Object.keys(byFeature).sort().forEach((k) => {
    fmapMd.push(`## ${k}`);
    byFeature[k].sort().forEach((f) => fmapMd.push(`- ${f}`));
    fmapMd.push('');
  });
  writeDoc('feature-map.md', fmapMd.join('\n'));
  writeDoc('feature-map-final.md', fmapMd.join('\n'));

  const shadow = files.filter((f) => shadowDirs.some((d) => f.startsWith(d + '/')));
  const staleRefs = findStaleRefs();
  const duplicates = findCriticalDuplicates(files);

  writeDoc('shadow-duplicates-report.md', [
    '# Shadow duplicates report', '', '## Shadow layer files',
    ...(shadow.length ? shadow.map((s) => `- ${s}`) : ['- none']), '',
    '## Critical basename duplicates',
    ...(duplicates.length ? duplicates.map((d) => `- ${d.name}: ${d.files.join(', ')}`) : ['- none'])
  ].join('\n'));

  const allowMatchers = loadDeadAllowlistMatchers();
  const deadCandidates = files.filter((f) => (rev[f] || []).length === 0);
  const dead = [];
  const allowlistedDead = [];
  deadCandidates.forEach((f) => {
    const m = matchAllowlist(f, allowMatchers);
    if (m) allowlistedDead.push({ file: f, pattern: m.pattern, reason: m.reason });
    else dead.push(f);
  });
  writeDoc('dead-code-report.md', [
    '# Dead code report', '',
    '## Unreferenced non-allowlisted runtime modules',
    ...(dead.length ? dead.map((x) => `- ${x}`) : ['- none']), '',
    '## Allowlisted unreferenced modules',
    ...(allowlistedDead.length ? allowlistedDead.map((x) => `- ${x.file} (pattern: ${x.pattern}; reason: ${x.reason})`) : ['- none'])
  ].join('\n') + '\n');

  const utilFiles = listFiles(root, { include: ['util/**/*.js'] });
  const utilLines = ['# Util audit report', ''];
  utilFiles.forEach((f) => {
    const [cls, note] = classifyUtil(f, graph, rev);
    utilLines.push(`- ${f}: ${cls} — ${note}`);
  });
  writeDoc('util-audit-report.md', utilLines.join('\n') + '\n');

  writeDoc('backend-layer-report.md', [
    '# Backend layer report', '',
    '- Canonical low-level client: `infra/odata/GatewayODataClient.js`.',
    '- Domain-facing backend client: `service/backend/GatewayClient.js`.',
    '- Adapter bridge layer: `infra/adapters/*` with shared helpers under `infra/adapters/shared/`.'
  ].join('\n'));

  writeDoc('full-audit-report.md', [
    '# Full audit report', '',
    '## Shadow layers', ...(shadow.length ? shadow.map((s) => `- ${s}`) : ['- none']), '',
    '## Stale tooling references', ...(staleRefs.length ? staleRefs.map((s) => `- ${s}`) : ['- none']), '',
    '## Duplicated critical basenames', ...(duplicates.length ? duplicates.map((d) => `- ${d.name}: ${d.files.length}`) : ['- none']), '',
    '## Risk assessment', '- Medium: dead-code and util classifications require incremental cleanup cycles.'
  ].join('\n'));

  writeDoc('cleanup-summary.md', [
    '# Cleanup summary', '',
    '- Generated dependency graph, reverse deps and feature map.',
    '- Reported shadow layers, stale refs and critical duplicates. Dead-code report is now allowlist-aware.',
    '- Updated util/backend audit snapshots for next cleanup cycle.'
  ].join('\n'));

  const validators = extractValidators();
  writeDoc('repo-discovery.md', [
    '# Repo discovery', '',
    '- QA command: `npm run qa` (pipeline) and `python3 scripts/qa-runner.py --changed --report docs/qa-report-latest.md` (report + cycle log).',
    '- Forbidden dirs: `mock_gate_way/`, `sap_backend/`.',
    '- Key validators source: `scripts/lib/qa-pipeline-config.js`.', '',
    '## Validators in scripts/lib/qa-pipeline-config.js',
    ...(validators.length ? validators.map((v) => `- ${v.name} (scripts/${v.file})`) : ['- none detected'])
  ].join('\n'));
})();
