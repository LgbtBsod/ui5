#!/usr/bin/env node

function predictRisk({ patch, impact, twin }) {
  const touched = (patch.files || []).map((f) => f.toLowerCase());
  const touchedState = patch.touchedStatePaths || [];

  const highSignal =
    touched.some((f) => /lock|autosave|cache/.test(f)) ||
    touchedState.includes('/lockOperationState') ||
    ((impact.workflowImpact || {}).portCallsChanged || []).length > 0;

  const medSignal =
    touched.some((f) => /statepaths|usecase|transition|detail\.controller|search\.controller/.test(f)) ||
    touchedState.includes('/mode');

  let level = 'LOW';
  if (highSignal) level = 'HIGH';
  else if (medSignal) level = 'MED';

  const history = ((twin && twin.snapshotsHistory) || []).slice(-10);
  const highIncidents = history.filter((h) => String(h.risk || '').toUpperCase() === 'HIGH').length;
  if (highIncidents >= 2 && level === 'MED') level = 'HIGH';

  return {
    riskLevel: level,
    rules: {
      HIGH: 'touches lock/autosave/cache sequencing or lock state paths/port calls',
      MED: 'touches state paths/transitions/usecases',
      LOW: 'formatting, import hygiene, docs'
    },
    signals: {
      touchedFiles: patch.files || [],
      touchedStatePaths: touchedState,
      touchedWorkflows: (impact.workflowImpact || {}).touchedWorkflows || [],
      predictedLayerViolations: (((impact || {}).layerImpact || {}).predictedLayerViolations || []).length,
      highIncidentPatternCount: highIncidents
    }
  };
}

module.exports = { predictRisk };
