#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { buildTwin, saveTwin } = require('./twin-builder');
const { parseGitDiff, parseProposedPatchFile, buildVirtualGraph } = require('./patch-sandbox');
const { simulateImpact } = require('./impact-simulator');
const { predictRisk } = require('./risk-predictor');
const { suggestSplit } = require('./safe-split-suggester');
const { writeReports } = require('./report-generator');

const THRESHOLDS = {
  invariantRiskThreshold: 0,
  predictedLayerViolationsMax: 0
};

function resolvePatch() {
  const arg = process.argv[2];
  if (arg && fs.existsSync(arg)) {
    return parseProposedPatchFile(arg);
  }
  return parseGitDiff();
}

function main() {
  const twin = buildTwin();
  const patch = resolvePatch();
  const virtualGraph = buildVirtualGraph(twin, patch);
  const impact = simulateImpact(twin, virtualGraph);
  const risk = predictRisk({ patch, impact, twin });
  const split = suggestSplit(patch, risk);

  twin.lastPrediction = {
    generatedAt: new Date().toISOString(),
    riskLevel: risk.riskLevel,
    touchedFiles: patch.files,
    touchedStatePaths: patch.touchedStatePaths
  };
  saveTwin(twin);
  writeReports({ twin, patch, impact, risk, split });

  const invariantRiskValue = impact.invariantImpact.predictedInvariantRisk ? 1 : 0;
  const failHighWithoutPlan = risk.riskLevel === 'HIGH' && !fs.existsSync(path.join(process.cwd(), 'docs/digital-twin/mitigation-plan.md'));
  const failInvariant = invariantRiskValue > THRESHOLDS.invariantRiskThreshold;
  const failLayer = impact.layerImpact.predictedLayerViolations.length > THRESHOLDS.predictedLayerViolationsMax;

  console.log(`digital-twin preflight: risk=${risk.riskLevel} layerViolations=${impact.layerImpact.predictedLayerViolations.length} invariantRisk=${invariantRiskValue}`);
  if (failHighWithoutPlan || failInvariant || failLayer) {
    console.error('FAIL digital-twin:preflight thresholds exceeded');
    process.exit(1);
  }
  console.log('PASS digital-twin:preflight');
}

if (require.main === module) main();

module.exports = { THRESHOLDS, main };
