#!/usr/bin/env node
const path = require('path');
const { readJsonSafe, readTextSafe } = require('../lib/io-runtime');

const ROOT = path.resolve(__dirname, '..', '..');

function readArchitectureHealthScore() {
  const text = readTextSafe(path.join(ROOT, 'docs', 'architecture-health.md'));
  const match = text.match(/architecture_health_score:\s*\*\*(\d+(?:\.\d+)?)\/100\*\*|- score:\s*(\d+(?:\.\d+)?)/i);
  return match ? Number(match[1] || match[2]) : null;
}

function readCompositeArchitectureScore() {
  const twinSummary = readTextSafe(path.join(ROOT, 'docs', 'digital-twin', 'twin-summary.md'));
  const architectAudit = readJsonSafe(path.join(ROOT, 'docs', 'artifacts', 'architect-audit-report.json'), null);
  const freeze = readJsonSafe(path.join(ROOT, 'docs', 'artifacts', 'final-architecture-freeze.json'), null);
  const health = readArchitectureHealthScore();
  const twinMatch = twinSummary.match(/ArchitectureScore:\s*\*\*(\d+(?:\.\d+)?)\*\*|ArchitectureScore:\s*(\d+(?:\.\d+)?)/i);
  const twin = twinMatch ? Number(twinMatch[1] || twinMatch[2]) : null;
  const parts = [
    { value: twin, weight: 0.35 },
    { value: architectAudit && architectAudit.readinessScore, weight: 0.30 },
    { value: freeze && freeze.ok ? freeze.score : null, weight: 0.20 },
    { value: health, weight: 0.15 }
  ].filter((item) => Number.isFinite(item.value));
  if (!parts.length) {
    return 0;
  }
  const totalWeight = parts.reduce((sum, item) => sum + item.weight, 0);
  return Math.round(parts.reduce((sum, item) => sum + item.value * item.weight, 0) / totalWeight);
}

function checkPolicies() {
  const violations = [];

  const dupTxt = readTextSafe(path.join(ROOT, 'docs', 'duplicate-logic.md'));
  const dupM = dupTxt.match(/Detected duplicate clusters:\s*(\d+)/i);
  const dup = dupM ? Number(dupM[1]) : 0;
  const dupPolicy = readTextSafe(path.join(ROOT, 'law', 'policies', 'duplication-policy.yml'));
  const dupMaxM = dupPolicy.match(/groups_max:\s*(\d+)/);
  const dupMax = dupMaxM ? Number(dupMaxM[1]) : 30;
  if (dup > dupMax) violations.push({ rule: 'duplication-policy', msg: `duplication groups ${dup} > ${dupMax}` });

  const econLedger = readTextSafe(path.join(ROOT, 'economy', 'ledger', 'ledger.jsonl')).trim().split('\n').filter(Boolean);
  if (econLedger.length) {
    const last = JSON.parse(econLedger[econLedger.length - 1]);
    if (Number(last.balanceAfter) < 0) violations.push({ rule: 'economy-policy', msg: `economy balance negative (${last.balanceAfter})` });
  }

  const budgetPolicy = readTextSafe(path.join(ROOT, 'udos', 'policies', 'architecture-budget.yml'));
  const archMinM = budgetPolicy.match(/architecture_min:\s*(\d+)/);
  const archMin = archMinM ? Number(archMinM[1]) : 92;
  const archScore = readCompositeArchitectureScore();
  if (archScore < archMin) violations.push({ rule: 'architecture-budget', msg: `ArchitectureScore ${archScore} < ${archMin}` });

  return { policyOk: violations.length === 0, violations };
}

module.exports = { checkPolicies };
