const { createIssue } = require('../lib/gateIssueFactory');

function createDeadCodeIssue(file, patchPath) {
  const spec = {};
  spec.ruleId = 'dead-code.unreferenced-module';
  spec.severity = 'HIGH';
  spec.file = file;
  spec.message = 'Module has no reverse dependencies in static runtime graph.';
  spec.evidence = `reverseDeps[${file}] = 0`;
  spec.fixHint = 'Migrate callsites or delete only after runtime verification and QA green.';
  spec.goodExample = 'service/domain/search/SearchFacade.js (has reverse deps)';
  spec.badExample = `${file} (0 static importers)`;
  spec.doc = 'docs/qa-rules/dead-code.unreferenced-module.md';
  spec.suggestedPatch = patchPath ? { path: patchPath, unifiedDiff: 'see scripts/autofix/out' } : undefined;
  return createIssue(spec);
}

function createDependencyBaselineMissingIssue() {
  const spec = {};
  spec.ruleId = 'drift.forbidden-edge';
  spec.severity = 'BLOCKER';
  spec.file = 'docs/deps-graph-baseline.json';
  spec.message = 'Missing dependency drift baseline.';
  spec.evidence = 'Baseline file not found.';
  spec.fixHint = 'Initialize explicitly: node scripts/ci/dependency-drift-gate.js --init-baseline';
  spec.goodExample = 'docs/deps-graph-baseline.json committed and reviewed.';
  spec.badExample = 'Baseline auto-created in CI without review.';
  spec.doc = 'docs/qa-rules/drift.forbidden-edge.md';
  return createIssue(spec);
}

function createDependencyRegressionIssue(entry) {
  const item = entry || {};
  const kind = item.kind;
  const spec = {};
  spec.ruleId = 'drift.forbidden-edge';
  spec.severity = 'BLOCKER';
  spec.file = item.file || '';
  spec.message = `New forbidden edge detected: ${item.edge}`;
  spec.evidence = `${kind}`;
  spec.fixHint = kind === 'usecase-to-sap-ui'
    ? 'Move UI5 imports to controller/adapter layer and keep usecases UI-agnostic.'
    : 'Inject facade/domain abstraction instead of importing infra/backend into controller.';
  spec.goodExample = 'controller/Search.controller.js -> service/domain/search/SearchFacade';
  spec.badExample = item.edge;
  spec.doc = 'docs/qa-rules/drift.forbidden-edge.md';
  return createIssue(spec);
}

module.exports = {
  createDeadCodeIssue,
  createDependencyBaselineMissingIssue,
  createDependencyRegressionIssue
};
