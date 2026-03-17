#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { scanFile } = require('../lib/js-deps-scanner');
const { listFiles } = require('../lib/fileWalker');
const { createGateResult, exitWithGateResult, finalizeAndExit } = require('../lib/gate-result');
const { createDependencyBaselineMissingIssue, createDependencyRegressionIssue } = require('./gate-issue-presets');
const { readJsonSafe } = require('../lib/auditInput');

const root = path.resolve(__dirname, '../..');
const baseline = path.join(root, 'docs/deps-graph-baseline.json');


function forbiddenEdge(file, dep) {
  if (/^controller\//.test(file) && /^checklist\/app\/(infra|service\/backend)\//.test(dep)) return 'controller-to-infra-backend';
  if (/^service\/domain\/.+\/usecases\/.+\.js$/.test(file) && /^sap\/ui\//.test(dep)) return 'usecase-to-sap-ui';
  return null;
}

(function main() {
  const asJson = process.argv.includes('--json');
  const files = ['controller', 'service', 'manager'].flatMap((d) => listFiles(root, { include: [`${d}/*.js`, `${d}/**/*.js`] })).sort();
  const edges = [];
  files.forEach((f) => scanFile(f, { rootDir: root }).forEach((d) => {
    const kind = forbiddenEdge(f, d.dep);
    if (kind) edges.push({ edge: `${f} -> ${d.dep}`, kind, file: f, dep: d.dep });
  }));
  const uniq = [...new Map(edges.map((e) => [e.edge, e])).values()].sort((a, b) => a.edge.localeCompare(b.edge));

  if (process.argv.includes('--init-baseline')) {
    fs.mkdirSync(path.dirname(baseline), { recursive: true });
    fs.writeFileSync(baseline, JSON.stringify({ forbiddenEdges: uniq.map((e) => e.edge) }, null, 2));
    const initResult = createGateResult('dependency-drift-gate', [], { initialized: true, baseline, edges: uniq.length });
    return finalizeAndExit(initResult, { asJson });
  }

  if (!fs.existsSync(baseline)) {
    return exitWithGateResult(
      'dependency-drift-gate',
      [createDependencyBaselineMissingIssue()],
      { filesScanned: files.length },
      { asJson, advisory: true }
    );
  }

  const base = (readJsonSafe(baseline, { forbiddenEdges: [] }).forbiddenEdges) || [];
  const regressions = uniq.filter((e) => !base.includes(e.edge));
  const errors = regressions.map((e) => createDependencyRegressionIssue(e));

  exitWithGateResult('dependency-drift-gate', errors, { filesScanned: files.length, forbiddenEdgesCurrent: uniq.length, regressions: regressions.length }, { asJson, advisory: true });
})();
