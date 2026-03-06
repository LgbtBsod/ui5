#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { readJsonLinesSafe, readJsonSafe } = require('../../lib/io-runtime');

const ROOT = path.resolve(__dirname, '..', '..', '..');
const PROFILES = path.join(ROOT, 'udos', 'memory', 'models', 'module-risk-profiles.json');
const EVENTS = path.join(ROOT, 'udos', 'memory', 'events', 'udos-events.jsonl');
const OUT_JSON = path.join(ROOT, 'udos', 'memory', 'policy-proposals', 'proposals.json');

function run() {
  const profiles = readJsonSafe(PROFILES, { profiles: [] }).profiles || [];
  const events = readJsonLinesSafe(EVENTS, []).filter((e) => e.type === 'flight_result');
  const proposals = [];

  const highModules = profiles.filter((p) => p.riskScore >= 1.7).slice(0, 5);
  for (const p of highModules) {
    proposals.push({
      id: `PP-${p.module}-proof`,
      type: 'STRICTER_PROOF_REQUIREMENT',
      target: p.module,
      recommendationMode: true,
      rationale: `Module ${p.module} riskScore=${p.riskScore} with patterns: ${(p.topRiskyPatterns || []).join(', ')}`,
      suggestedChange: 'Require proof mode + exclusive window for /mode and /lockOperationState changes.'
    });
  }

  const dupUp = events.slice(-10).filter((e) => Number(e.duplication_delta || 0) > 0).length >= 4;
  if (dupUp) {
    proposals.push({
      id: 'PP-dup-threshold',
      type: 'DUPLICATION_THRESHOLD_TIGHTEN',
      recommendationMode: true,
      rationale: 'Duplication delta positive in >=4 of last 10 flights.',
      suggestedChange: 'Lower duplication_groups_max and create extraction mission batches.'
    });
  }

  const allGreen = events.slice(-10).length > 0 && events.slice(-10).every((e) => e.qa_passed && e.invariants_passed && !e.deny_reason);
  if (allGreen) {
    proposals.push({
      id: 'PP-relax-low-risk',
      type: 'RELAX_LOW_RISK_REVIEW',
      recommendationMode: true,
      rationale: 'Last 10 flights are stable green.',
      suggestedChange: 'Slightly relax review overhead for LOW-risk non-runtime-only patches.'
    });
  }

  const stamp = new Date().toISOString().slice(0, 10);
  const mdFile = path.join(ROOT, 'udos', 'memory', 'policy-proposals', `proposals-${stamp}.md`);
  const md = `# Policy Update Proposals (${stamp})\n\n${proposals.map((p) => `## ${p.id}\n- Type: ${p.type}\n- Recommendation mode: ${p.recommendationMode}\n- Target: ${p.target || '-'}\n- Rationale: ${p.rationale}\n- Suggested change: ${p.suggestedChange}`).join('\n\n') || 'No proposals.'}\n`;

  fs.writeFileSync(OUT_JSON, JSON.stringify({ generatedAt: new Date().toISOString(), proposals }, null, 2) + '\n');
  fs.writeFileSync(mdFile, md);
  console.log(`PASS policy-proposer proposals=${proposals.length}`);
}

if (require.main === module) run();

module.exports = { run };
