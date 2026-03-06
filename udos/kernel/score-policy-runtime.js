const fs = require('fs');
const path = require('path');
const { readJsonSafe, readTextSafe } = require('../lib/io-runtime');

function readArchitectureHealthScore(rootDir) {
  const file = path.join(rootDir, 'docs', 'architecture-health.md');
  if (!fs.existsSync(file)) {
    return null;
  }
  const text = readTextSafe(file, '');
  const match = text.match(/architecture_health_score:\s*\*\*(\d+(?:\.\d+)?)\/100\*\*|- score:\s*(\d+(?:\.\d+)?)/i);
  return match ? Number(match[1] || match[2]) : null;
}

function computeCompositeArchitectureScore(rootDir, baseScore) {
  const architectAudit = readJsonSafe(path.join(rootDir, 'docs', 'artifacts', 'architect-audit-report.json'), null);
  const freeze = readJsonSafe(path.join(rootDir, 'docs', 'artifacts', 'final-architecture-freeze.json'), null);
  const architectureHealth = readArchitectureHealthScore(rootDir);
  const parts = [
    { value: baseScore, weight: 0.35 },
    { value: architectAudit && architectAudit.readinessScore, weight: 0.30 },
    { value: freeze && freeze.ok ? freeze.score : null, weight: 0.20 },
    { value: architectureHealth, weight: 0.15 }
  ].filter((item) => Number.isFinite(item.value));
  if (!parts.length) {
    return baseScore;
  }
  const totalWeight = parts.reduce((sum, item) => sum + item.weight, 0);
  const weighted = parts.reduce((sum, item) => sum + item.value * item.weight, 0);
  return Math.round(weighted / totalWeight);
}

function parseBudgetPolicy(rootDir) {
  const file = path.join(rootDir, 'udos', 'policies', 'architecture-budget.yml');
  const txt = fs.readFileSync(file, 'utf8');
  const getNum = (k, d) => {
    const m = txt.match(new RegExp(`${k}:\\s*(\\d+)`));
    return m ? Number(m[1]) : d;
  };
  return {
    architecture_min: getNum('architecture_min', 92),
    ail_min: getNum('ail_min', 88),
    pmi_min: getNum('pmi_min', 82),
    adt_min: getNum('adt_min', 85),
    domain_min: getNum('domain_min', 85),
    duplication_groups_max: getNum('duplication_groups_max', 20),
    high_risk_flights_per_week_max: getNum('high_risk_flights_per_week_max', 2)
  };
}

function readTwinSummaryScores(rootDir, defaults) {
  const sumFile = path.join(rootDir, 'docs', 'digital-twin', 'twin-summary.md');
  const score = { ...(defaults || {}) };
  if (!fs.existsSync(sumFile)) return score;
  const t = fs.readFileSync(sumFile, 'utf8');
  const get = (k, d) => {
    const m = t.match(new RegExp(`${k}:\\s*\\*\\*(\\d+(?:\\.\\d+)?)\\*\\*|${k}:\\s*(\\d+(?:\\.\\d+)?)`, 'i'));
    return m ? Number(m[1] || m[2]) : d;
  };
  score.ADT = get('ADT_SCORE', score.ADT);
  score.ArchitectureScore = get('ArchitectureScore', score.ArchitectureScore);
  score.AIL = get('AIL_Score', score.AIL);
  score.PMI = get('PMI', score.PMI);
  score.DomainCompleteness = get('DomainCompletenessScore', score.DomainCompleteness);
  score.ArchitectureScore = computeCompositeArchitectureScore(rootDir, score.ArchitectureScore);
  return score;
}

module.exports = {
  parseBudgetPolicy,
  readTwinSummaryScores
};
