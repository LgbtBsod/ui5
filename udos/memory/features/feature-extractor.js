#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { readJsonLinesSafe } = require('../../lib/io-runtime');

const ROOT = path.resolve(__dirname, '..', '..', '..');
const EVENTS = path.join(ROOT, 'udos', 'memory', 'events', 'udos-events.jsonl');
const VECTORS = path.join(ROOT, 'udos', 'memory', 'features', 'feature-vectors.jsonl');

function workflowFlags(files) {
  const f = files.map((x) => String(x).toLowerCase());
  return {
    lock: f.some((x) => x.includes('lock')) ? 1 : 0,
    autosave: f.some((x) => x.includes('autosave')) ? 1 : 0,
    cache: f.some((x) => x.includes('cache')) ? 1 : 0,
    search: f.some((x) => x.includes('search')) ? 1 : 0
  };
}

function moduleName(file) {
  const p = String(file || '').split('/');
  return p.length > 1 ? p[1] : p[0] || 'unknown';
}

function historicalStats(events, modules, workflows) {
  const hist = events.filter((e) => e.type === 'flight_result');
  const inModules = (e) => (e.modules || []).some((m) => modules.includes(m));
  const inWorkflows = (e) => (e.workflows || []).some((w) => workflows.includes(w));
  const modEvents = hist.filter(inModules);
  const wfEvents = hist.filter(inWorkflows);
  const fail = (arr, key) => arr.length ? arr.filter((e) => !e[key]).length / arr.length : 0;
  const deny = modEvents.length ? modEvents.filter((e) => !!e.deny_reason).length / modEvents.length : 0;
  const lastHigh = hist.filter((e) => (e.risk_level || '').toUpperCase() === 'HIGH' && inModules(e)).slice(-1)[0];
  const daysSinceHigh = lastHigh ? Math.floor((Date.now() - new Date(lastHigh.ts).getTime()) / 86400000) : 999;
  return {
    historical_failure_rate_module: fail(modEvents, 'qa_passed'),
    historical_deny_rate_module: deny,
    historical_invariant_failure_rate_workflow: fail(wfEvents, 'invariants_passed'),
    days_since_last_high_risk_change: daysSinceHigh
  };
}

function extractFeatureVector(event, allEvents) {
  const files = event.files_changed || [];
  const modules = [...new Set(files.map(moduleName))];
  const workflows = event.workflows || [];
  const touchedState = event.touched_state_paths || [];
  const wf = workflowFlags(files);
  const hist = historicalStats(allEvents, modules, workflows);
  const complexityDelta = Number((event.complexity_delta || 0));
  return {
    ts: event.ts,
    flight_id: event.flight_id,
    risk_level: event.risk_level,
    features: {
      loc_changed: Number(event.loc_changed || 0),
      files_touched: files.length,
      modules_touched: modules.length,
      workflow_lock: wf.lock,
      workflow_autosave: wf.autosave,
      workflow_cache: wf.cache,
      workflow_search: wf.search,
      touched_state_paths: touchedState,
      new_dependency_edges: Number(event.new_dependency_edges || 0),
      duplication_delta: Number(event.duplication_delta || 0),
      complexity_delta: complexityDelta,
      ...hist
    },
    modules: modules,
    files_changed: files,
    outcome: {
      qa_passed: !!event.qa_passed,
      invariants_passed: !!event.invariants_passed,
      denied: !!event.deny_reason
    }
  };
}

function run() {
  const events = readJsonLinesSafe(EVENTS, []);
  const vectors = events.filter((e) => e.type === 'flight_result').map((e) => extractFeatureVector(e, events));
  fs.writeFileSync(VECTORS, vectors.map((v) => JSON.stringify(v)).join('\n') + (vectors.length ? '\n' : ''));
  console.log(`PASS feature-extractor vectors=${vectors.length}`);
}

if (require.main === module) run();

module.exports = { extractFeatureVector, run };
