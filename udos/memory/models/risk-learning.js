#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { readJsonLinesSafe } = require('../../lib/io-runtime');

const ROOT = path.resolve(__dirname, '..', '..', '..');
const VECTORS = path.join(ROOT, 'udos', 'memory', 'features', 'feature-vectors.jsonl');
const MODEL_FILE = path.join(ROOT, 'udos', 'memory', 'models', 'risk_model.json');

function score(v, w) {
  const f = v.features || {};
  let s = 0;
  s += (f.files_touched || 0) * w.files_touched;
  s += (f.modules_touched || 0) * w.modules_touched;
  s += (f.new_dependency_edges || 0) * w.new_dependency_edges;
  s += ((f.workflow_lock || 0) + (f.workflow_autosave || 0) + (f.workflow_cache || 0)) * w.critical_workflow;
  s += ((f.touched_state_paths || []).includes('/mode') ? 1 : 0) * w.mode_path;
  s += ((f.touched_state_paths || []).includes('/lockOperationState') ? 1 : 0) * w.lock_path;
  s += Math.max(0, (f.duplication_delta || 0)) * w.duplication_growth;
  s += (f.historical_failure_rate_module || 0) * w.hist_failure;
  s += (f.historical_deny_rate_module || 0) * w.hist_deny;
  s += (f.historical_invariant_failure_rate_workflow || 0) * w.hist_invariant;
  return s;
}

function train(vectors) {
  const weights = {
    files_touched: 0.04,
    modules_touched: 0.08,
    new_dependency_edges: 0.15,
    critical_workflow: 0.9,
    mode_path: 0.7,
    lock_path: 1.0,
    duplication_growth: 0.2,
    hist_failure: 1.2,
    hist_deny: 1.3,
    hist_invariant: 1.4
  };

  // deterministic auto-tune by error direction
  for (const v of vectors) {
    const observedFail = (!v.outcome.qa_passed || !v.outcome.invariants_passed || v.outcome.denied) ? 1 : 0;
    const predicted = score(v, weights) >= 1.7 ? 1 : 0;
    const err = observedFail - predicted;
    if (err === 0) continue;
    weights.critical_workflow += 0.02 * err * ((v.features.workflow_lock || v.features.workflow_autosave || v.features.workflow_cache) ? 1 : 0);
    weights.lock_path += 0.03 * err * ((v.features.touched_state_paths || []).includes('/lockOperationState') ? 1 : 0);
    weights.mode_path += 0.02 * err * ((v.features.touched_state_paths || []).includes('/mode') ? 1 : 0);
    weights.hist_failure += 0.01 * err;
    weights.hist_deny += 0.01 * err;
  }

  const model = {
    trainedAt: new Date().toISOString(),
    method: 'weighted-heuristics-auto-tuned',
    thresholdDenyRisk: 1.7,
    weights
  };
  fs.writeFileSync(MODEL_FILE, JSON.stringify(model, null, 2) + '\n');
  return model;
}

function explain(v, model) {
  const f = v.features;
  const parts = [];
  if ((f.workflow_lock || f.workflow_autosave || f.workflow_cache)) parts.push('critical workflow touched');
  if ((f.touched_state_paths || []).includes('/lockOperationState')) parts.push('touches /lockOperationState');
  if ((f.touched_state_paths || []).includes('/mode')) parts.push('touches /mode');
  if ((f.historical_failure_rate_module || 0) > 0.2) parts.push('module has elevated historical failure rate');
  return parts;
}

function run() {
  const vectors = readJsonLinesSafe(VECTORS, []);
  const model = train(vectors);
  console.log(`PASS risk-learning vectors=${vectors.length} method=${model.method}`);
}

if (require.main === module) run();

module.exports = { train, explain, score };
