const path = require('path');
const { parseImports } = require('./js-import-parser');

function resolveLocal(file, dep) {
  if (dep.startsWith('sap/')) return null;
  if (!dep.startsWith('./') && !dep.startsWith('../')) return dep.replace(/^PRODUCTION_CONTROL_CHECKLIST\//, '').replace(/\.js$/, '');
  return path.posix.normalize(path.posix.join(path.posix.dirname(file), dep)).replace(/\.js$/, '');
}

function buildGraph(fileMap) {
  const graph = new Map();
  Object.keys(fileMap).forEach((file) => graph.set(file, []));
  for (const [file, text] of Object.entries(fileMap)) {
    const deps = parseImports(text).map((dep) => resolveLocal(file, dep)).filter(Boolean);
    graph.set(file, deps);
  }
  return graph;
}

function fanStats(graph) {
  const inMap = new Map();
  const outMap = new Map();
  for (const [file, deps] of graph.entries()) {
    outMap.set(file, deps.length);
    deps.forEach((dep) => inMap.set(dep, (inMap.get(dep) || 0) + 1));
  }
  return { fanIn: inMap, fanOut: outMap };
}

function detectCycles(graph) {
  const seen = new Set();
  const stack = new Set();
  const cycles = [];

  function dfs(node, trail) {
    if (stack.has(node)) {
      const idx = trail.indexOf(node);
      cycles.push(trail.slice(idx).concat(node));
      return;
    }
    if (seen.has(node)) return;
    seen.add(node);
    stack.add(node);
    (graph.get(node) || []).forEach((next) => dfs(next, trail.concat(next)));
    stack.delete(node);
  }

  Array.from(graph.keys()).forEach((key) => dfs(key, [key]));
  return cycles;
}

module.exports = { parseImports, buildGraph, fanStats, detectCycles };
