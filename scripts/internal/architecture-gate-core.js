#!/usr/bin/env node

const {
  readText,
  collectFilesRecursively,
  lineFromIndex,
  extractUi5Dependencies
} = require('../qa-shared');
const { readJsonSafe } = require('../lib/auditInput');
const { canonicalLayer, resolveDependency } = require('./layer-contract');

const ROOT = process.cwd();
const SCAN_DIRS = ['controller', 'facades', 'service', 'infra', 'util', 'model'];
const INFRA_BACKEND_ALLOWLIST = new Set(['service/backend/GatewayClient']);
const CONTROLLER_INFRA_ALLOWLIST = new Set([
  'infra/navigation/WorkspaceRouteNavigation'
]);
const CONTROLLER_UTIL_ALLOWLIST_PATH = 'scripts/internal/controller-util-allowlist.json';
const CONTROLLER_UTIL_ALLOWLIST = new Set(readJsonSafe(CONTROLLER_UTIL_ALLOWLIST_PATH, []));

function pushControllerViolations(violations, file, item, line, targetLayer, modulePath) {
  if (targetLayer === 'infra' || targetLayer === 'backend') {
    if (targetLayer === 'infra' && CONTROLLER_INFRA_ALLOWLIST.has(modulePath)) {
      return false;
    }
    violations.push({ rule: 'R1', from: file, dep: item.dep, line, msg: 'controller cannot import infra/backend' });
    return true;
  }
  if (targetLayer === 'util' && !CONTROLLER_UTIL_ALLOWLIST.has(modulePath)) {
    violations.push({ rule: 'R1', from: file, dep: item.dep, line, msg: 'controller util import is not allowlisted' });
    return true;
  }
  if (!['controller', 'facade', 'usecase', 'util'].includes(targetLayer)) {
    violations.push({ rule: 'R1', from: file, dep: item.dep, line, msg: 'controller import target is outside allowed layers' });
    return true;
  }
  return false;
}

function pushLayerViolations(violations, sourceLayer, targetLayer, file, item, line, modulePath) {
  if (sourceLayer === 'usecase' && targetLayer === 'controller') {
    violations.push({ rule: 'R2', from: file, dep: item.dep, line, msg: 'usecase cannot import controller' });
  }
  if (sourceLayer === 'facade' && ['controller', 'backend'].includes(targetLayer)) {
    violations.push({ rule: 'R2b', from: file, dep: item.dep, line, msg: 'facade cannot import controller/backend' });
  }
  if (sourceLayer === 'backend' && (targetLayer === 'controller' || targetLayer === 'usecase')) {
    violations.push({ rule: 'R3', from: file, dep: item.dep, line, msg: 'backend cannot import controller/usecase' });
  }
  if (sourceLayer === 'infra' && ['controller', 'usecase', 'backend'].includes(targetLayer)) {
    const allowed = targetLayer === 'backend' && INFRA_BACKEND_ALLOWLIST.has(modulePath);
    if (!allowed) violations.push({ rule: 'R4', from: file, dep: item.dep, line, msg: 'infra cannot import controller/usecase/backend' });
  }
}

function scanFileForViolations(file, fileSet, adjacency, violations) {
  const source = readText(ROOT, file);
  const sourceLayer = canonicalLayer(file);
  const deps = extractUi5Dependencies(source);
  adjacency.set(file, adjacency.get(file) || []);

  for (const item of deps) {
    const resolved = resolveDependency(ROOT, file, item.dep, '.');
    const targetLayer = resolved.kind === 'local' ? resolved.layerTag : 'external';
    const line = lineFromIndex(source, item.index);
    if (resolved.filePath && fileSet.has(resolved.filePath)) adjacency.get(file).push(resolved.filePath);
    if (resolved.kind === 'ui5' || resolved.kind === 'external') continue;
    if (sourceLayer === 'controller' && pushControllerViolations(violations, file, item, line, targetLayer, resolved.modulePath)) continue;
    pushLayerViolations(violations, sourceLayer, targetLayer, file, item, line, resolved.modulePath);
  }
}

function findViolations(files) {
  const violations = [];
  const adjacency = new Map();
  const fileSet = new Set(files);
  for (const file of files) scanFileForViolations(file, fileSet, adjacency, violations);
  return { violations, adjacency };
}

function detectCycles(files, adjacency) {
  const state = new Map();
  const stack = [];
  const indexMap = new Map();
  const cycles = [];

  function pushCycle(fromNode, toNode) {
    const start = indexMap.get(toNode);
    if (start == null) return;
    const cycle = stack.slice(start).concat([toNode]);
    const key = cycle.join('->');
    if (!cycles.some((c) => c.key === key)) {
      cycles.push({ key, chain: cycle });
    }
  }

  function dfs(node) {
    state.set(node, 1);
    indexMap.set(node, stack.length);
    stack.push(node);

    const neighbors = adjacency.get(node) || [];
    for (const next of neighbors) {
      if (!files.includes(next)) continue;
      const st = state.get(next) || 0;
      if (st === 0) {
        dfs(next);
      } else if (st === 1) {
        pushCycle(node, next);
      }
    }

    stack.pop();
    indexMap.delete(node);
    state.set(node, 2);
  }

  for (const file of files) {
    if ((state.get(file) || 0) === 0) {
      dfs(file);
    }
  }

  return cycles.map((c) => c.chain);
}

function main() {
  const files = [];
  for (const dir of SCAN_DIRS) {
    collectFilesRecursively(ROOT, dir, (rel) => rel.endsWith('.js'), files);
  }

  const uniqueFiles = [...new Set(files)].sort();
  const { violations, adjacency } = findViolations(uniqueFiles);
  const cycles = detectCycles(uniqueFiles, adjacency);

  for (const chain of cycles) {
    violations.push({
      rule: 'R5',
      from: chain[0],
      dep: chain.join(' -> '),
      line: null,
      msg: 'dependency cycle detected'
    });
  }

  if (violations.length) {
    console.log('FAIL architecture-gate');
    for (const v of violations) {
      const linePart = v.line ? `:${v.line}` : '';
      console.log(`- ${v.rule} | ${v.from}${linePart} | ${v.dep} | ${v.msg}`);
    }
    process.exit(1);
  }

  console.log('PASS architecture-gate');
  process.exit(0);
}

main();
