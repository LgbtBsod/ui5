#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(__dirname, '..', '..');

function ensureDir(p) { if (!fs.existsSync(p)) fs.mkdirSync(p, { recursive: true }); }

function writeReports({ twin, patch, impact, risk, split }) {
  const outDir = path.join(ROOT, 'docs', 'digital-twin');
  ensureDir(outDir);

  const bigPatch = (patch.files || []).length > 12;
  const impactMd = `# Digital Twin Impact Report

## Patch summary
- Touched files: ${patch.files.length}
- Virtual edges added: ${patch.addedImports.length}
- Predicted risk level: **${risk.riskLevel}**

## Predicted impact
- Layer violations: ${impact.layerImpact.predictedLayerViolations.length}
- Workflow lock/autosave/cache touched: ${impact.workflowImpact.lockAutosaveCacheTouched}
- Invariant risk: ${impact.invariantImpact.predictedInvariantRisk}
- Technical debt deltaEdges: ${impact.technicalDebtImpact.deltaEdges}
- Duplication change: ${impact.dedupeImpact.duplicationClustersBefore} -> ${impact.dedupeImpact.predictedAfter} (delta ${impact.dedupeImpact.predictedDelta})
- God helper risk: ${impact.dedupeImpact.godHelperRisk}

## Affected modules
${patch.files.map((f) => `- ${f}`).join('\n') || '- (none)'}

## Affected workflows
${impact.summary.affectedWorkflows.map((w) => `- ${w}`).join('\n') || '- (none)'}

## Recommended PR batch split
${bigPatch ? 'Patch is large: split is recommended.' : 'Patch size is moderate: split optional.'}
${split.batches.map((b) => `- Batch ${b.order} [${b.level}]: ${b.files.length} files`).join('\n')}
`;

  const riskMd = `# Digital Twin Risk Report

## Predicted risk
- Level: **${risk.riskLevel}**

## Rules
- HIGH: ${risk.rules.HIGH}
- MED: ${risk.rules.MED}
- LOW: ${risk.rules.LOW}

## Signals
- Touched state paths: ${(risk.signals.touchedStatePaths || []).join(', ') || '-'}
- Touched workflows: ${(risk.signals.touchedWorkflows || []).join(', ') || '-'}
- Predicted layer violations: ${risk.signals.predictedLayerViolations}
- Past high-incident pattern count: ${risk.signals.highIncidentPatternCount}

## Safe split suggestion
${split.batches.map((b) => `### Batch ${b.order} — ${b.level}
${b.files.map((f) => `- ${f}`).join('\n') || '- (none)'}

${b.note}`).join('\n\n')}
`;

  const summaryMd = `# Digital Twin Summary

- Generated at: ${twin.generatedAt}
- ADT_SCORE: **${twin.metrics.ADT_SCORE}**
- ArchitectureScore: ${twin.metrics.ArchitectureScore}
- AIL_Score: ${twin.metrics.AIL_Score}
- PMI: ${twin.metrics.PMI}
- DomainCompletenessScore: ${twin.metrics.DomainCompletenessScore}

## ADT_SCORE Formula
ADT_SCORE = (ArchitectureScore + AIL_Score + PMI + DomainCompletenessScore) / 4

## Trend
${(twin.metricsTrend || []).map((x) => `- ${x.date}: ADT_SCORE=${x.ADT_SCORE}`).join('\n') || '- baseline only'}
`;

  fs.writeFileSync(path.join(outDir, 'impact-report.md'), impactMd);
  fs.writeFileSync(path.join(outDir, 'risk-report.md'), riskMd);
  fs.writeFileSync(path.join(outDir, 'twin-summary.md'), summaryMd);
}

module.exports = { writeReports };
