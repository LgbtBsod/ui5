#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { score } = require('./risk-learning');
const { readJsonLinesSafe, readJsonSafe } = require('../../lib/io-runtime');

const ROOT = path.resolve(__dirname, '..', '..', '..');
const VECTORS = path.join(ROOT, 'udos', 'memory', 'features', 'feature-vectors.jsonl');
const MODEL_FILE = path.join(ROOT, 'udos', 'memory', 'models', 'risk_model.json');
const OUT = path.join(ROOT, 'udos', 'memory', 'models', 'module-risk-profiles.json');

function run() {
  const vectors = readJsonLinesSafe(VECTORS, []);
  const model = readJsonSafe(MODEL_FILE, { weights: {} });
  const map = {};
  for (const v of vectors) {
    const modules = v.modules && v.modules.length ? v.modules : ['unknown'];
    for (const m of modules) {
      map[m] = map[m] || { module: m, scoreSamples: [], riskyPatterns: {}, topRiskyFiles: {} };
      map[m].scoreSamples.push(score(v, model.weights || {}));
      for (const f of (v.files_changed || [])) {
        if (String(f).split('/').includes(m)) map[m].topRiskyFiles[f] = (map[m].topRiskyFiles[f] || 0) + 1;
      }
      if ((v.features.touched_state_paths || []).includes('/mode')) map[m].riskyPatterns['mutates_/mode'] = (map[m].riskyPatterns['mutates_/mode'] || 0) + 1;
      if ((v.features.touched_state_paths || []).includes('/lockOperationState')) map[m].riskyPatterns['mutates_/lockOperationState'] = (map[m].riskyPatterns['mutates_/lockOperationState'] || 0) + 1;
      if ((v.features.workflow_lock || v.features.workflow_autosave || v.features.workflow_cache)) map[m].riskyPatterns['critical_workflow_touch'] = (map[m].riskyPatterns['critical_workflow_touch'] || 0) + 1;
    }
  }

  const profiles = Object.values(map).map((x) => ({
    module: x.module,
    riskScore: Number((x.scoreSamples.reduce((a, b) => a + b, 0) / Math.max(1, x.scoreSamples.length)).toFixed(3)),
    topRiskyFiles: Object.entries(x.topRiskyFiles).sort((a,b)=>b[1]-a[1]).slice(0,5).map((e)=>e[0]),
    topRiskyPatterns: Object.entries(x.riskyPatterns).sort((a, b) => b[1] - a[1]).map((e) => e[0])
  })).sort((a, b) => b.riskScore - a.riskScore);

  const payload = { generatedAt: new Date().toISOString(), profiles };
  fs.writeFileSync(OUT, JSON.stringify(payload, null, 2) + '\n');
  console.log(`PASS module-risk-profiles count=${profiles.length}`);
}

if (require.main === module) run();

module.exports = { run };
