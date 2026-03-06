#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');
const { getChangedFiles } = require('../../scripts/lib/git-changes');
const cfg = require('./config');
const logger = require('./logger');
const { emit } = require('./event-bus');
const { loadState, saveState } = require('./state-store');
const { buildSchedule } = require('./scheduler');
const { writeDashboards: writeDashboardsArtifacts } = require('./dashboard-writer');
const ScorePolicyRuntime = require('./score-policy-runtime');
const { readJsonSafe, readTextSafe } = require('../lib/io-runtime');

function runCmd(cmd) {
  try {
    const out = execSync(cmd, { encoding: 'utf8', stdio: 'pipe' });
    return { ok: true, out };
  } catch (e) {
    return { ok: false, out: (e.stdout || '') + (e.stderr || '') };
  }
}

function hasNpmScript(scriptName) {
  const pkgFile = path.join(cfg.ROOT, 'package.json');
  const pkg = readJsonSafe(pkgFile, null);
  return !!(pkg && pkg.scripts && pkg.scripts[scriptName]);
}

function parseBudgetPolicy() {
  return ScorePolicyRuntime.parseBudgetPolicy(cfg.ROOT);
}

function readTwinSummaryScores() {
  return ScorePolicyRuntime.readTwinSummaryScores(cfg.ROOT, cfg.defaults.scores);
}

function classifyChange(files) {
  if (files.some((f) => /manifest|package\.json|scripts\//.test(f))) return 'infra';
  if (files.some((f) => /hotfix|bug|fix/i.test(f))) return 'hotfix';
  if (files.some((f) => /view\//.test(f))) return 'feature';
  return 'refactor';
}

function getChangedFilesSafe() {
  const files = getChangedFiles();
  return Array.isArray(files) ? files : [];
}

function intake(state) {
  const files = getChangedFilesSafe();
  const risk = files.some((f) => /lock|autosave|cache/i.test(f)) ? 'HIGH' : files.some((f) => /controller|usecases|statepaths/i.test(f)) ? 'MED' : 'LOW';
  const intakeData = {
    files,
    changeType: classifyChange(files),
    impactedWorkflows: ['searchWorkflow', 'detailOpenWorkflow'].filter((w) => files.some((f) => w.toLowerCase().includes(f.split('/')[1] || ''))),
    risk
  };
  state.lastIntake = intakeData;
  emit('change_detected', intakeData);
  return state;
}

function plan(state) {
  const flightId = `FP-${new Date().toISOString().slice(0, 10)}-${String((state.queue || []).length + 1).padStart(3, '0')}`;
  const fp = {
    flightPlanId: flightId,
    risk: (state.lastIntake && state.lastIntake.risk) || 'MED',
    files: (state.lastIntake && state.lastIntake.files) || [],
    hasPreflight: false
  };
  state.queue = state.queue || [];
  state.queue.push(fp);
  emit('flight_plan_generated', fp);
  return state;
}

function simulate(state) {
  const r = runCmd('npm run digital-twin:preflight');
  const proofRequired = (state.lastIntake && state.lastIntake.risk === 'HIGH');
  const proofPresent = fs.existsSync(path.join(cfg.ROOT, 'docs', 'digital-twin', 'mitigation-plan.md'));
  const ok = r.ok && (!proofRequired || proofPresent);
  const sim = { ok, proofRequired, proofPresent, output: r.out.slice(-600) };
  state.simulations = state.simulations || [];
  state.simulations.push(sim);
  emit('preflight_completed', sim);
  if (proofRequired && !proofPresent) emit('policy_violation', { reason: 'proof-mode required for high risk' });
  return state;
}

function approve(state) {
  const pol = parseBudgetPolicy();
  const scores = readTwinSummaryScores();
  state.scores = scores;
  const budgetReasons = [];
  if (scores.ArchitectureScore < pol.architecture_min) budgetReasons.push('architecture score below min');
  if (scores.AIL < pol.ail_min) budgetReasons.push('AIL score below min');
  if (scores.PMI < pol.pmi_min) budgetReasons.push('PMI below min');
  if (scores.ADT < pol.adt_min) budgetReasons.push('ADT below min');
  if (scores.DomainCompleteness < pol.domain_min) budgetReasons.push('Domain completeness below min');

  const lastSim = (state.simulations || []).slice(-1)[0] || { ok: false };
  let decision = 'CLEAR';
  if (!lastSim.ok || budgetReasons.length) decision = 'DENY';
  else if ((state.lastIntake && state.lastIntake.risk === 'HIGH')) decision = 'CLEAR WITH CONDITIONS';

  const d = { at: new Date().toISOString(), decision, reasons: budgetReasons };
  state.decisions = state.decisions || [];
  state.decisions.push(d);
  state.budget = { violated: budgetReasons.length > 0, reasons: budgetReasons };
  if (budgetReasons.length) emit('budget_exceeded', { reasons: budgetReasons });
  if (decision === 'DENY') emit('policy_violation', d);
  else emit('clearance_granted', d);

  return state;
}

function execute(state) {
  const risk = (state.lastIntake && state.lastIntake.risk) || 'MED';
  const action = risk === 'LOW' ? 'AUTO_APPLY_SAFE_FIXES' : 'ASSISTED_PATCHSETS_ONLY';
  state.lastExecute = { action, risk };
  return state;
}

function verify(state) {
  const cmds = ['npm run qa', 'npm run architect:audit', 'npm run domain-model:verify', 'npm run digital-twin:preflight', 'npm run air-traffic-control'];
  const optional = ['npm run architecture-intel'];
  const results = cmds.map((c) => ({ cmd: c, ...runCmd(c) }));
  const opt = optional.map((c) => {
    const m = String(c).match(/^npm run (.+)$/);
    if (m && !hasNpmScript(m[1])) {
      return { cmd: c, ok: false, optional: true, skipped: true, out: `optional script missing: ${m[1]}` };
    }
    return { cmd: c, ...runCmd(c), optional: true };
  });
  state.lastVerify = { results, optional: opt };
  const ok = results.every((r) => r.ok);
  emit('postflight_completed', { ok });
  return state;
}

function missionEngine(state) {
  const missionsFile = cfg.missionsFile;
  let missions = readJsonSafe(missionsFile, []);

  const pol = parseBudgetPolicy();
  const duplicateMd = path.join(cfg.ROOT, 'docs', 'duplicate-logic.md');
  const txt = readTextSafe(duplicateMd, '');
  const m = txt.match(/Detected duplicate clusters:\s*(\d+)/i);
  const dup = m ? Number(m[1]) : 0;
  if (dup > pol.duplication_groups_max || (state.budget && state.budget.violated)) {
    const mission = {
      id: `MIS-${new Date().toISOString().slice(0, 10)}-${String(missions.length + 1).padStart(3, '0')}`,
      title: 'Reduce duplication / recover architecture budget',
      reason: dup > pol.duplication_groups_max ? `duplication ${dup} > ${pol.duplication_groups_max}` : 'budget violated',
      status: 'OPEN'
    };
    missions.push(mission);
    emit('mission_created', mission);
  }

  fs.mkdirSync(path.dirname(missionsFile), { recursive: true });
  fs.writeFileSync(missionsFile, JSON.stringify(missions, null, 2) + '\n');
  state.missions = missions;
  return state;
}

function writeDashboards(state) {
  writeDashboardsArtifacts(state, cfg, buildSchedule);
}

function runMemoryTraining(state) {
  runCmd('node udos/memory/events/consolidate-events.js');
  runCmd('node udos/memory/features/feature-extractor.js');
  runCmd('node udos/memory/models/risk-learning.js');
  runCmd('node udos/memory/models/module-risk-profiles.js');
  runCmd('node udos/memory/policy-proposals/policy-proposer.js');
  return state;
}

function adaptiveGovern(state) {
  runMemoryTraining(state);
  const profilesFile = path.join(cfg.ROOT, 'udos', 'memory', 'models', 'module-risk-profiles.json');
  const profiles = readJsonSafe(profilesFile, { profiles: [] }).profiles || [];
  const intake = state.lastIntake || { files: [], risk: 'LOW' };
  const modules = [...new Set((intake.files || []).map((f) => String(f).split('/')[1] || String(f).split('/')[0]))];
  const risky = profiles.filter((p) => modules.includes(p.module) && p.riskScore >= 1.7);
  const needsProof = intake.risk === 'HIGH' || risky.length > 0;
  const proofPresent = fs.existsSync(path.join(cfg.ROOT, 'docs', 'digital-twin', 'mitigation-plan.md'));

  state.adaptive = {
    riskyModules: risky,
    requireProofMode: needsProof,
    requireExclusiveWindow: needsProof,
    requireSmallerBatches: needsProof,
    explanation: risky.length ? `Historical risk for modules: ${risky.map((r) => r.module).join(', ')}` : 'Static policy fallback'
  };

  if (needsProof && !proofPresent) {
    const d = { at: new Date().toISOString(), decision: 'DENY', reasons: ['adaptive governance requires proof mode'] };
    state.decisions = state.decisions || [];
    state.decisions.push(d);
    emit('policy_violation', { reason: 'adaptive proof required', modules: modules });
  }
  return state;
}


function economyGovern(state) {
  const intake = state.lastIntake || { files: [], risk: 'LOW' };
  const modules = [...new Set((intake.files || []).map((f) => String(f).split('/')[1] || String(f).split('/')[0]))];
  const ctx = {
    flightId: (state.queue || []).slice(-1)[0]?.flightPlanId || `FP-${new Date().toISOString().slice(0,10)}-ECO`,
    changeType: intake.changeType || 'refactor',
    modules,
    files: intake.files || [],
    locChanged: 0,
    lockTouched: (intake.files || []).some((f)=>/lock/i.test(f)),
    touchedStatePaths: [],
    newLayerEdges: 0,
    riskLevel: intake.risk || 'LOW',
    historicalFailRate: 0,
    exclusiveWindowRequired: (intake.risk || '').toUpperCase() === 'HIGH',
    duplicationDelta: 0,
    controllerThresholdExceeded: false,
    layerViolations: 0,
    budgetExceeded: !!(state.budget && state.budget.violated),
    godHelperAdded: false,
    controllerLocReduced: 0,
    layerViolationsFixed: 0,
    invariantScenariosAdded: 0,
    adtDelta: 0
  };
  const r = runCmd(`node udos/economy/engine/economy-engine.js`);
  const breakdownFile = path.join(cfg.ROOT, 'udos', 'economy', 'reports', 'pr-cost-breakdown.md');
  const economyReport = path.join(cfg.ROOT, 'udos', 'economy', 'reports', 'economy-report.md');
  if (fs.existsSync(breakdownFile)) {
    fs.copyFileSync(breakdownFile, path.join(cfg.ROOT, 'udos', 'reports', 'pr-cost-breakdown.md'));
  }
  if (fs.existsSync(path.join(cfg.ROOT, 'udos', 'economy', 'reports', 'ledger-summary.md'))) {
    fs.copyFileSync(path.join(cfg.ROOT, 'udos', 'economy', 'reports', 'ledger-summary.md'), path.join(cfg.ROOT, 'udos', 'reports', 'ledger-summary.md'));
  }
  if (fs.existsSync(economyReport)) {
    fs.copyFileSync(economyReport, path.join(cfg.ROOT, 'udos', 'reports', 'economy-report.md'));
  }

  const econText = readTextSafe(economyReport, '');
  const deny = /Decision:\s*DENY/.test(econText);
  if (deny) {
    const d = { at: new Date().toISOString(), decision: 'DENY', reasons: ['economy balance insufficient'] };
    state.decisions = state.decisions || [];
    state.decisions.push(d);
    emit('policy_violation', { reason: 'economy deny' });
  }
  emit('score_updated', { subsystem: 'economy', status: r.ok ? 'ok' : 'fail' });
  return state;
}

function runMode(mode, state) {
  state.lastMode = mode;
  switch (mode) {
    case 'intake': return intake(state);
    case 'plan': return plan(state);
    case 'simulate': return simulate(state);
    case 'approve': return approve(state);
    case 'execute': return execute(state);
    case 'verify': return verify(state);
    case 'report': writeDashboards(state); return state;
    case 'govern': return missionEngine(approve(simulate(state)));
    case 'govern-adaptive': return adaptiveGovern(missionEngine(approve(simulate(state))));
    case 'govern-economy': return economyGovern(missionEngine(approve(simulate(state))));
    default: return state;
  }
}

function main() {
  const args = process.argv.slice(2);
  const mode = args[0] === '--' ? args[1] : args[0];
  const adaptive = args.includes('--adaptive') || args[2] === '--adaptive';
  const economy = args.includes('--economy') || args[2] === '--economy';
  let target = mode || 'full';
  if (adaptive && mode === 'govern') target = 'govern-adaptive';
  if (economy && mode === 'govern') target = 'govern-economy';
  const pipeline = target === 'full'
    ? ['intake', 'plan', 'simulate', 'approve', 'execute', 'verify', 'govern', 'report']
    : [target];

  let state = loadState();
  for (const m of pipeline) {
    logger.info(`Running mode ${m}`);
    state = runMode(m, state);
    saveState(state);
  }

  const lastDecision = (state.decisions || []).slice(-1)[0] || { decision: 'CLEAR' };
  const deny = lastDecision.decision === 'DENY' || (state.budget && state.budget.violated);
  if (deny) {
    logger.error('UDOS DENY');
    process.exit(1);
  }
  logger.info('UDOS PASS', { mode: target });
}

if (require.main === module) main();

module.exports = { main, runMode };

