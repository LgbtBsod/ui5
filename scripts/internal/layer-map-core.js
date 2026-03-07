#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const {
  detectRuntimeRoot,
  collectFilesByExtensions,
  readText,
  extractUi5Dependencies
} = require('../qa-shared');
const {
  canonicalLayer,
  toLayerName,
  normalizeRuntimeRelative,
  resolveDependency
} = require('./layer-contract');

const ROOT = process.cwd();
const RUNTIME_ROOT = detectRuntimeRoot(ROOT);
const LAYER_DIRS = ['controller', 'facades', 'service/domain', 'service/backend', 'infra', 'util'];
const CONTROLLER_INFRA_ALLOWLIST = new Set([
  'infra/navigation/WorkspaceRouteNavigation'
]);

function rp(rel) {
  return normalizeRuntimeRelative('.', path.join(RUNTIME_ROOT, rel));
}

function listRuntimeJsFiles() {
  return collectFilesByExtensions(ROOT, LAYER_DIRS.map(rp), ['.js']);
}

function layerByRuntimeFile(relFile) {
  return toLayerName(canonicalLayer(normalizeRuntimeRelative(RUNTIME_ROOT, relFile)));
}

function collectGraphAndViolations(files) {
  const graph = new Map();
  const violations = [];
  const layerEntries = {
    Controllers: [],
    Facades: [],
    Usecases: [],
    Backend: [],
    Infra: [],
    Util: []
  };

  for (const file of files) {
    const src = readText(ROOT, file);
    const fromNoPrefix = normalizeRuntimeRelative(RUNTIME_ROOT, file);
    const fromLayer = layerByRuntimeFile(file);

    if (!graph.has(file)) graph.set(file, []);

    const imports = [];
    extractUi5Dependencies(src).forEach((item) => {
      const dep = resolveDependency(ROOT, file, item.dep, RUNTIME_ROOT);
      imports.push({ raw: item.dep, resolved: dep.modulePath, layer: dep.layerName });
      if (dep.filePath) graph.get(file).push(dep.filePath);

      if (
        fromLayer === 'Controllers'
        && dep.layerName === 'Infra'
        && CONTROLLER_INFRA_ALLOWLIST.has(dep.modulePath)
      ) {
        return;
      }
      if (fromLayer === 'Controllers' && (dep.layerName === 'Infra' || dep.layerName === 'Backend')) {
        violations.push(`controller imports ${dep.layerName.toLowerCase()}: ${fromNoPrefix} -> ${dep.modulePath}`);
      }
      if (fromLayer === 'Infra' && (dep.layerName === 'Controllers' || dep.layerName === 'Usecases')) {
        violations.push(`infra imports ${dep.layerName.toLowerCase().replace(/s$/, '')}: ${fromNoPrefix} -> ${dep.modulePath}`);
      }
    });

    if (layerEntries[fromLayer]) {
      layerEntries[fromLayer].push({ file: fromNoPrefix, imports });
    }
  }

  return { graph, violations, layerEntries };
}

function detectCycles(allFiles, graph) {
  const visited = new Set();
  const inStack = new Set();
  const stack = [];
  const cycles = [];

  function dfs(node) {
    visited.add(node);
    inStack.add(node);
    stack.push(node);

    (graph.get(node) || []).forEach((next) => {
      if (!graph.has(next)) return;
      if (!visited.has(next)) {
        dfs(next);
      } else if (inStack.has(next)) {
        const i = stack.indexOf(next);
        if (i >= 0) {
          cycles.push(stack.slice(i).concat(next).map((item) => normalizeRuntimeRelative(RUNTIME_ROOT, item)));
        }
      }
    });

    stack.pop();
    inStack.delete(node);
  }

  allFiles.forEach((file) => {
    if (!visited.has(file)) dfs(file);
  });

  const seen = new Set();
  return cycles.filter((cycle) => {
    const key = cycle.join(' -> ');
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
}

function sectionLines(title, entries) {
  const lines = [`## ${title}`, ''];
  if (!entries.length) {
    lines.push('- (no files)', '');
    return lines;
  }

  entries.sort((a, b) => a.file.localeCompare(b.file)).forEach((entry) => {
    lines.push(`- \`${entry.file}\``);
    if (!entry.imports.length) {
      lines.push('  - imports: (none)');
      return;
    }
    entry.imports.forEach((item) => lines.push(`  - \`${item.raw}\` -> \`${item.resolved}\` [${item.layer}]`));
  });
  lines.push('');
  return lines;
}

function buildMarkdown(layerEntries, violations) {
  const md = [];
  md.push('# Architecture map', '');
  md.push(`Generated from runtime root: \`${RUNTIME_ROOT}\`.`, '');
  md.push(...sectionLines('Controllers -> (imported modules)', layerEntries.Controllers));
  md.push(...sectionLines('Facades -> (imported modules)', layerEntries.Facades));
  md.push(...sectionLines('Usecases -> (imported modules)', layerEntries.Usecases));
  md.push(...sectionLines('Backend -> (imported modules)', layerEntries.Backend));
  md.push(...sectionLines('Infra -> (imported modules)', layerEntries.Infra));
  md.push(...sectionLines('Util -> (imported modules)', layerEntries.Util));
  md.push('## Violations', '');
  if (!violations.length) md.push('- none');
  else violations.forEach((v) => md.push(`- ${v}`));
  md.push('');
  return md.join('\n');
}

function main() {
  const allFiles = [...new Set(listRuntimeJsFiles())].sort();
  const { graph, violations, layerEntries } = collectGraphAndViolations(allFiles);
  detectCycles(allFiles, graph).forEach((cycle) => violations.push(`cycle: ${cycle.join(' -> ')}`));

  const outPath = path.join(ROOT, 'docs', 'architecture-map.md');
  fs.mkdirSync(path.dirname(outPath), { recursive: true });
  fs.writeFileSync(outPath, buildMarkdown(layerEntries, violations), 'utf8');

  const artifact = normalizeRuntimeRelative('.', path.relative(ROOT, outPath));
  if (!violations.length) {
    console.log('LAYER-MAP PASS');
    console.log(`artifact: ${artifact}`);
    return;
  }

  console.log('LAYER-MAP FAIL');
  console.log(`artifact: ${artifact}`);
  violations.forEach((v) => console.log(`- ${v}`));
  process.exit(1);
}

main();
