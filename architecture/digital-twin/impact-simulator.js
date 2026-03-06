#!/usr/bin/env node

const LAYER_RULES = {
  controller: ['controller', 'service/framework', 'service/domain', 'util', 'model', 'facades', 'ports'],
  'service/domain': ['service/domain', 'service/framework', 'util', 'ports', 'model'],
  infra: ['infra', 'service/framework', 'util', 'ports', 'model'],
  manager: ['manager', 'service/domain', 'service/framework', 'util', 'model', 'ports'],
  util: ['util', 'model', 'service/framework', 'ports']
};

function layerPrefix(file) {
  if (String(file).startsWith('controller/')) return 'controller';
  if (String(file).startsWith('service/domain/')) return 'service/domain';
  if (String(file).startsWith('infra/')) return 'infra';
  if (String(file).startsWith('manager/')) return 'manager';
  if (String(file).startsWith('util/')) return 'util';
  return 'other';
}

function isAllowedEdge(from, to) {
  const lf = layerPrefix(from);
  if (!LAYER_RULES[lf]) return true;
  return LAYER_RULES[lf].some((prefix) => String(to).includes(prefix) || String(to).startsWith(prefix));
}

function predictLayerViolations(virtualGraph) {
  const violations = [];
  const touchedSet = new Set(virtualGraph.touchedFiles || []);
  for (const e of virtualGraph.dependencyEdges || []) {
    const isCandidate = e.kind === "virtual-import" || touchedSet.has(e.from);
    if (!isCandidate) continue;
    if (!String(e.to).startsWith("sap_ui5/") && !String(e.to).startsWith("infra/") && !String(e.to).startsWith("service/") && !String(e.to).startsWith("controller/") && !String(e.to).startsWith("util/") && !String(e.to).startsWith("ports/") && !String(e.to).startsWith("manager/")) {
      continue;
    }
    if (!isAllowedEdge(e.from, e.to)) {
      violations.push({ type: "FORBIDDEN_EDGE", edge: e });
    }
  }
  return violations;
}

function simulateImpact(twin, virtualGraph) {
  const touched = virtualGraph.touchedFiles || [];
  const lower = touched.map((f) => f.toLowerCase());

  const touchedPortCalls = (virtualGraph.dependencyEdges || [])
    .filter((e) => touched.includes(e.from) && /\/ports\//.test(String(e.to).toLowerCase()))
    .map((e) => ({ from: e.from, to: e.to }));

  const workflowImpact = {
    lockAutosaveCacheTouched: lower.some((f) => /lock|autosave|cache/.test(f)),
    touchedUseCases: virtualGraph.touchedUseCases || touched.filter((f) => f.includes('/usecases/')),
    stateTransitionRisk: (virtualGraph.touchedStatePaths || []).some((p) => ['/mode', '/lockOperationState'].includes(p)),
    portCallsChanged: touchedPortCalls,
    touchedWorkflows: virtualGraph.touchedWorkflows || []
  };

  const layerViolations = predictLayerViolations(virtualGraph);
  const invariantRisk = workflowImpact.stateTransitionRisk;

  const beforeDup = Number((twin.metrics && twin.metrics.duplicationClusters) || 0);
  const helperTouches = touched.filter((f) => /helper|mixin/i.test(f));
  const dedupeImpact = {
    duplicationClustersBefore: beforeDup,
    predictedDelta: helperTouches.length > 0 ? -1 : 0,
    predictedAfter: beforeDup + (helperTouches.length > 0 ? -1 : 0),
    godHelperRisk: helperTouches.length >= 3
  };

  return {
    layerImpact: {
      predictedLayerViolations: layerViolations,
      boundaryRisk: layerViolations.length > 0
    },
    workflowImpact,
    invariantImpact: {
      predictedInvariantRisk: invariantRisk,
      touchedStatePaths: virtualGraph.touchedStatePaths || []
    },
    technicalDebtImpact: {
      deltaEdges: (virtualGraph.metrics && virtualGraph.metrics.deltaEdges) || 0,
      touchedFilesCount: touched.length
    },
    dedupeImpact,
    summary: {
      affectedModules: touched,
      affectedWorkflows: workflowImpact.touchedWorkflows.length
        ? workflowImpact.touchedWorkflows
        : (twin.workflowGraph || [])
          .filter((w) => /lock|autosave|cache|detail|search/i.test(w.workflow))
          .map((w) => w.workflow)
    }
  };
}

module.exports = { simulateImpact, predictLayerViolations };
