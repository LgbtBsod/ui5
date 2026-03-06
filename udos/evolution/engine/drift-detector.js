#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { readJsonSafe, readTextSafe } = require('../../lib/io-runtime');

function movingTrend(values) {
  if (!values.length) return 0;
  if (values.length === 1) return values[0];
  const half = Math.max(1, Math.floor(values.length / 2));
  const first = values.slice(0, half).reduce((a, b) => a + b, 0) / half;
  const secondValues = values.slice(-half);
  const second = secondValues.reduce((a, b) => a + b, 0) / secondValues.length;
  if (first === 0) return second > 0 ? 100 : 0;
  return ((second - first) / Math.abs(first)) * 100;
}

function parseCourtViolations(verdictText) {
  if (!verdictText) return [];
  const lines = verdictText.split('\n');
  const out = [];
  let inViolations = false;
  for (const line of lines) {
    if (line.startsWith('Policy violations:')) {
      inViolations = true;
      continue;
    }
    if (inViolations && line.trim() === '') break;
    if (inViolations && line.trim().startsWith('- ') && !line.includes('none')) {
      const raw = line.trim().slice(2);
      const rule = raw.split(':')[0];
      out.push(rule);
    }
  }
  return out;
}

function detectDrift(input) {
  const history = (input && input.history) || [];
  const riskProfiles = (input && input.riskProfiles) || [];
  const courtViolations = (input && input.courtViolations) || [];

  const duplicationTrend = movingTrend(history.map((s) => Number(s.duplicationGroups || 0)));
  const controllerTrend = movingTrend(history.map((s) => Number(s.controllerComplexity || 0)));

  const policyFreq = {};
  for (const v of courtViolations) policyFreq[v] = (policyFreq[v] || 0) + 1;

  const riskyModules = riskProfiles
    .filter((p) => Number(p.riskScore || 0) >= 0.65)
    .map((p) => ({ module: p.module, riskScore: p.riskScore }));

  const signals = [];
  if (duplicationTrend >= 15) {
    signals.push({ module: 'global', signal: `duplication trend +${duplicationTrend.toFixed(1)}%`, risk: 'medium' });
  }
  if (controllerTrend >= 20) {
    signals.push({ module: 'detail', signal: `controller complexity trend +${controllerTrend.toFixed(1)}%`, risk: 'medium' });
  }
  for (const [rule, count] of Object.entries(policyFreq)) {
    if (count >= 3) signals.push({ module: 'policy', signal: `frequent violations: ${rule} (${count})`, risk: 'high' });
  }
  for (const m of riskyModules) {
    signals.push({ module: m.module, signal: `high-risk module change pressure (${Math.round(m.riskScore * 100)}%)`, risk: m.riskScore > 0.8 ? 'high' : 'medium' });
  }

  const modulesAtRisk = [...new Set(signals.map((s) => s.module).filter((m) => m !== 'global' && m !== 'policy'))];
  const mission = signals.length
    ? {
        id: 'MISSION-CONTROLLER-THINNING',
        goal: 'reduce controller LOC',
        expectedScoreDelta: 4,
        risk: 'low'
      }
    : null;

  return { driftDetected: signals.length > 0, signals, modulesAtRisk, proposedMission: mission };
}

function loadAndDetect(rootDir) {
  const historyFile = path.join(rootDir, 'udos', 'evolution', 'models', 'architecture-history.json');
  const risksFile = path.join(rootDir, 'udos', 'memory', 'models', 'module-risk-profiles.json');
  const courtFile = path.join(rootDir, 'udos', 'reports', 'court-verdict.md');

  const history = readJsonSafe(historyFile, { snapshots: [] }).snapshots || [];
  const riskProfiles = readJsonSafe(risksFile, { profiles: [] }).profiles || [];
  const courtViolations = parseCourtViolations(readTextSafe(courtFile, ''));

  return detectDrift({ history, riskProfiles, courtViolations });
}

module.exports = { detectDrift, loadAndDetect };
