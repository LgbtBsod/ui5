#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { readJsonSafe } = require('../../lib/io-runtime');

const ROOT = path.resolve(__dirname, '..', '..', '..');
const OUT = path.join(ROOT, 'udos', 'memory', 'events', 'udos-events.jsonl');

function run() {
  const events = [];
  const state = readJsonSafe(path.join(ROOT, 'udos', 'history', 'udos-state.json'), {});
  const sims = state.simulations || [];
  const decisions = state.decisions || [];
  const intake = state.lastIntake || {};
  const scoresAfter = state.scores || {};

  const flightId = (state.queue || []).slice(-1)[0]?.flightPlanId || `FP-${new Date().toISOString().slice(0,10)}-000`;
  const qaPassed = !!((state.lastVerify && state.lastVerify.results || []).find((r) => r.cmd === 'npm run qa' && r.ok));
  const invPassed = !!((state.lastVerify && state.lastVerify.results || []).find((r) => r.cmd === 'npm run digital-twin:preflight' && r.ok));
  const deny = (decisions.slice(-1)[0] && decisions.slice(-1)[0].decision === 'DENY') ? 'GOVERNANCE_DENY' : null;

  events.push({
    ts: new Date().toISOString(),
    type: 'flight_result',
    flight_id: flightId,
    risk_level: intake.risk || 'MED',
    modules: [...new Set((intake.files || []).map((f) => String(f).split('/')[1] || String(f).split('/')[0]))],
    workflows: intake.impactedWorkflows || [],
    files_changed: intake.files || [],
    touched_state_paths: [],
    scores_before: {},
    scores_after: scoresAfter,
    qa_passed: qaPassed,
    invariants_passed: invPassed,
    deny_reason: deny,
    violations: {
      budget_violated: !!(state.budget && state.budget.violated),
      reasons: (state.budget && state.budget.reasons) || []
    },
    new_dependency_edges: 0,
    complexity_delta: 0,
    loc_changed: 0,
    duplication_delta: 0,
    simulation_ok: sims.slice(-1)[0]?.ok || false
  });

  fs.writeFileSync(OUT, events.map((e) => JSON.stringify(e)).join('\n') + '\n');
  console.log(`PASS consolidate-events events=${events.length}`);
}

if (require.main === module) run();

module.exports = { run };
