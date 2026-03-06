#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

function generateRfc(params) {
  const drift = params.drift;
  const patterns = params.patterns;

  const hasDrift = drift && drift.driftDetected;
  const topAnti = ((patterns && patterns.antiPatterns) || []).sort((a, b) => b.occurrences - a.occurrences)[0];
  const rfcId = `RFC-${new Date().toISOString().slice(0, 10).replace(/-/g, '')}-ARCH-02`;

  const title = hasDrift
    ? 'Introduce LockResultHandlers as canonical lock transition handler'
    : 'No mandatory architecture evolution changes';

  const reason = hasDrift
    ? `architectural drift detected (${drift.signals.length} signals), top anti-pattern: ${topAnti ? `${topAnti.pattern} (${topAnti.occurrences})` : 'n/a'}.`
    : 'no active drift signals above threshold.';

  const proposal = hasDrift
    ? [
        'extract repeated lock transition handling into domain/platform/LockResultHandlers',
        'run mission MISSION-CONTROLLER-THINNING to reduce controller LOC',
        'propose policy update for controller complexity trend alerting'
      ]
    : ['continue monitoring and collect additional architecture snapshots'];

  const impact = hasDrift
    ? '- duplication -8\n- architectureScore +3\n- controller complexity trend normalization'
    : '- neutral impact expected';

  return { rfcId, title, reason, proposal, impact };
}

function writeRfc(rootDir, rfc) {
  const file = path.join(rootDir, 'udos', 'evolution', 'reports', 'evolution-rfc.md');
  const content = `# ${rfc.rfcId}\n\nTitle: ${rfc.title}\n\nReason:\n${rfc.reason}\n\nProposal:\n${rfc.proposal.map((p) => `- ${p}`).join('\n')}\n\nExpected impact:\n${rfc.impact}\n\nGovernance note:\n- Evolution Engine does not modify runtime code or constitution automatically.\n- Output is advisory: RFC + mission + policy-change proposals only.\n`;
  fs.writeFileSync(file, content);
}

module.exports = { generateRfc, writeRfc };
