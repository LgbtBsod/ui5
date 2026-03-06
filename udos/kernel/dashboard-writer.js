const fs = require('fs');
const path = require('path');
const { readJsonLinesSafe, readJsonSafe } = require('../lib/io-runtime');

function writeDashboards(state, cfg, buildSchedule) {
  const ddir = path.join(cfg.ROOT, 'udos', 'dashboards');
  const rdir = path.join(cfg.ROOT, 'udos', 'reports');
  fs.mkdirSync(ddir, { recursive: true });
  fs.mkdirSync(rdir, { recursive: true });

  buildSchedule();
  fs.writeFileSync(path.join(ddir, 'udos-dashboard.md'), `# UDOS Dashboard\n\n- Mode: ${state.lastMode || '-'}\n- Last decision: ${(state.decisions || []).slice(-1)[0]?.decision || '-'}\n- Queue size: ${(state.queue || []).length}\n- Missions: ${(state.missions || []).length}\n- Budget violated: ${state.budget?.violated || false}\n\n## Lifecycle\nintake -> plan -> simulate -> approve -> execute -> verify -> learn\n`);
  fs.writeFileSync(path.join(ddir, 'scores.md'), `# UDOS Scores\n\n- ArchitectureScore: ${state.scores?.ArchitectureScore}\n- AIL: ${state.scores?.AIL}\n- PMI: ${state.scores?.PMI}\n- ADT: ${state.scores?.ADT}\n- DomainCompleteness: ${state.scores?.DomainCompleteness}\n`);
  fs.writeFileSync(path.join(ddir, 'budget-status.md'), `# Budget Status\n\n- Violated: ${state.budget?.violated || false}\n- Reasons: ${(state.budget?.reasons || []).join(', ') || '-'}\n`);
  fs.writeFileSync(path.join(ddir, 'flights-queue.md'), `# Flights Queue\n\n${(state.queue || []).map((f, i) => `${i + 1}. ${f.flightPlanId} (${f.risk})`).join('\n') || '- none'}\n`);
  fs.writeFileSync(path.join(ddir, 'missions.md'), `# Missions\n\n${(state.missions || []).map((m) => `- ${m.id}: ${m.title} [${m.status}]`).join('\n') || '- none'}\n`);
  fs.writeFileSync(path.join(ddir, 'hot-modules.md'), '# Hot Modules\n\n- service/domain/detail/usecases\n- controller/\n- manager/\n');
  fs.writeFileSync(path.join(ddir, 'duplication.md'), '# Duplication\n\nSee docs/duplicate-logic.md and ADT impact reports.\n');

  const profilesFile = path.join(cfg.ROOT, 'udos', 'memory', 'models', 'module-risk-profiles.json');
  const propsFile = path.join(cfg.ROOT, 'udos', 'memory', 'policy-proposals', 'proposals.json');
  const memEventsFile = path.join(cfg.ROOT, 'udos', 'memory', 'events', 'udos-events.jsonl');
  const profiles = readJsonSafe(profilesFile, { profiles: [] }).profiles || [];
  const proposals = readJsonSafe(propsFile, { proposals: [] }).proposals || [];
  const memEvents = readJsonLinesSafe(memEventsFile, []);
  const topPredictors = ['critical_workflow_touch', 'mutates_/lockOperationState', 'mutates_/mode'];
  const denyReasons = (state.decisions || []).filter((d) => d.decision === 'DENY').flatMap((d) => d.reasons || []);
  fs.writeFileSync(path.join(ddir, 'memory-dashboard.md'), `# Memory Dashboard\n\n## Top risky modules (last 30 days)\n${profiles.slice(0, 5).map((p) => `- ${p.module}: ${p.riskScore}`).join('\n') || '- none'}\n\n## Top failure predictors\n${topPredictors.map((p) => `- ${p}`).join('\n')}\n\n## Top deny reasons\n${[...new Set(denyReasons)].map((r) => `- ${r}`).join('\n') || '- none'}\n\n## Score trend + correlation with risk\n- ADT=${state.scores?.ADT}\n- Recorded memory events=${memEvents.length}\n\n## Recommended policy changes\n${proposals.map((p) => `- ${p.id}: ${p.suggestedChange}`).join('\n') || '- none'}\n`);

  const lastSim = (state.simulations || []).slice(-1)[0] || {};
  const lastDecision = (state.decisions || []).slice(-1)[0] || { decision: '-', reasons: [] };
  const analysis = {
    generatedAt: new Date().toISOString(),
    mode: state.lastMode || '-',
    decision: lastDecision.decision,
    decisionReasons: lastDecision.reasons || [],
    budget: state.budget || { violated: false, reasons: [] },
    scores: state.scores || {},
    queueSize: (state.queue || []).length,
    openMissions: (state.missions || []).filter((m) => m.status === 'OPEN').length,
    lastIntake: state.lastIntake || { files: [], changeType: '-', impactedWorkflows: [], risk: 'LOW' },
    verification: {
      required: (state.lastVerify && state.lastVerify.results) || [],
      optional: (state.lastVerify && state.lastVerify.optional) || []
    },
    simulation: {
      ok: !!lastSim.ok,
      proofRequired: !!lastSim.proofRequired,
      proofPresent: !!lastSim.proofPresent
    },
    adaptive: state.adaptive || null,
    memory: {
      recordedEvents: memEvents.length,
      topRiskModules: profiles.slice(0, 5).map((p) => ({ module: p.module, riskScore: p.riskScore })),
      policyProposals: proposals.slice(0, 5).map((p) => ({ id: p.id, suggestedChange: p.suggestedChange }))
    }
  };
  const analysisMd = [
    '# UDOS Analysis Report',
    '',
    `- Generated at: ${analysis.generatedAt}`,
    `- Mode: ${analysis.mode}`,
    `- Decision: ${analysis.decision}`,
    `- Budget violated: ${analysis.budget.violated}`,
    `- Queue size: ${analysis.queueSize}`,
    `- Open missions: ${analysis.openMissions}`,
    '',
    '## Scores',
    `- ArchitectureScore: ${analysis.scores.ArchitectureScore ?? '-'}`,
    `- AIL: ${analysis.scores.AIL ?? '-'}`,
    `- PMI: ${analysis.scores.PMI ?? '-'}`,
    `- ADT: ${analysis.scores.ADT ?? '-'}`,
    `- DomainCompleteness: ${analysis.scores.DomainCompleteness ?? '-'}`,
    '',
    '## Last Intake',
    `- Change type: ${analysis.lastIntake.changeType || '-'}`,
    `- Risk: ${analysis.lastIntake.risk || '-'}`,
    `- Impacted workflows: ${(analysis.lastIntake.impactedWorkflows || []).join(', ') || '-'}`,
    `- Files: ${(analysis.lastIntake.files || []).join(', ') || '-'}`,
    '',
    '## Verification',
    ...(((analysis.verification.required || []).map((r) => `- ${r.cmd}: ${r.ok ? 'PASS' : 'FAIL'}`)).length
      ? (analysis.verification.required || []).map((r) => `- ${r.cmd}: ${r.ok ? 'PASS' : 'FAIL'}`)
      : ['- not run']),
    '',
    '## Top Risk Modules',
    ...((analysis.memory.topRiskModules || []).length
      ? analysis.memory.topRiskModules.map((m) => `- ${m.module}: ${m.riskScore}`)
      : ['- none']),
    '',
    '## Policy Proposals',
    ...((analysis.memory.policyProposals || []).length
      ? analysis.memory.policyProposals.map((p) => `- ${p.id}: ${p.suggestedChange}`)
      : ['- none'])
  ].join('\n') + '\n';

  fs.writeFileSync(path.join(rdir, 'simulation-report.md'), `# UDOS Simulation Report\n\n- Result: ${lastSim.ok ? 'PASS' : 'FAIL'}\n- Proof required: ${!!lastSim.proofRequired}\n- Proof present: ${!!lastSim.proofPresent}\n- Output tail:\n\n\`\`\`\n${lastSim.output || ''}\n\`\`\`\n`);
  fs.writeFileSync(path.join(rdir, 'governance-report.md'), `# UDOS Governance Report\n\n- Decision: ${(state.decisions || []).slice(-1)[0]?.decision || '-'}\n- Reasons: ${((state.decisions || []).slice(-1)[0]?.reasons || []).join(', ') || '-'}\n- Budget violated: ${state.budget?.violated || false}\n`);
  fs.writeFileSync(path.join(rdir, 'verification-report.md'), `# UDOS Verification Report\n\n${(state.lastVerify?.results || []).map((r) => `- ${r.cmd}: ${r.ok ? 'PASS' : 'FAIL'}`).join('\n') || '- not run'}\n\nOptional:\n${(state.lastVerify?.optional || []).map((r) => `- ${r.cmd}: ${r.ok ? 'PASS' : (r.skipped ? 'SKIP (optional)' : 'FAIL (optional)')}`).join('\n') || '-'}\n`);
  fs.writeFileSync(path.join(rdir, 'analysis-report.md'), analysisMd);
  fs.writeFileSync(path.join(rdir, 'analysis-report.json'), JSON.stringify(analysis, null, 2) + '\n');
}

module.exports = {
  writeDashboards
};
