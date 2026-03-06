#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { checkConstitution } = require('./constitution-checker');
const { checkPolicies } = require('./policy-checker');
const { checkCaseLaw } = require('./case-law-checker');
const { checkAppeals } = require('./appeal-engine');
const { buildVerdict } = require('./verdict-engine');

const ROOT = path.resolve(__dirname, '..');

function bulletList(items, mapItem) {
  if (!items || !items.length) return '- none';
  return items.map((item) => `- ${mapItem(item)}`).join('\n');
}

function run() {
  const constitution = checkConstitution();
  const policy = checkPolicies();
  const caseLaw = checkCaseLaw(policy.violations || []);
  const appeals = checkAppeals(caseLaw.unmet || []);
  const verdict = buildVerdict({ constitution, policy, caseLaw, appeals });

  const outReport = path.join(ROOT, 'reports', 'court-verdict.md');
  const outDash = path.join(ROOT, 'dashboards', 'court-dashboard.md');
  fs.mkdirSync(path.dirname(outReport), { recursive: true });
  fs.mkdirSync(path.dirname(outDash), { recursive: true });

  const report = `# ARCHITECTURE COURT VERDICT

Case: PR-LOCAL
Constitution: ${constitution.constitutionalOk ? 'OK' : 'VIOLATED'}
Policies: ${policy.policyOk ? 'OK' : 'violations found'}
Precedent: ${(caseLaw.allowedByPrecedent || []).length ? 'found' : 'none'}
Appeal: ${(appeals.accepted || []).length ? 'accepted' : 'none'}

VERDICT: ${verdict.verdict}

Conditions:
${bulletList(verdict.conditions, (c) => c)}

Reasons:
${bulletList(verdict.reasons, (r) => r)}

Policy violations:
${bulletList(policy.violations, (v) => `${v.rule}: ${v.message}`)}

Precedent overrides:
${bulletList(caseLaw.allowedByPrecedent, (c) => `${c.case.case_id} -> ${c.violation.rule}`)}

Unmet violations:
${bulletList(caseLaw.unmet, (v) => `${v.rule}: ${v.message}`)}
`;

  const dash = `# Court Dashboard

## constitutional violations
${bulletList(constitution.violations, (v) => v)}

## active appeals
${bulletList(appeals.accepted, (a) => `${a.appeal.appeal_id} (${a.appeal.rule})`)}

## precedent cases
${bulletList(caseLaw.allowedByPrecedent, (c) => `${c.case.case_id} for ${c.violation.rule}`)}

## unmet violations
${bulletList(caseLaw.unmet, (v) => `${v.rule}: ${v.message}`)}

## denied PRs
${verdict.verdict === 'DENY' || verdict.verdict === 'UNCONSTITUTIONAL' ? '- PR-LOCAL' : '- none'}
`;

  fs.writeFileSync(outReport, report);
  fs.writeFileSync(outDash, dash);

  const explainFile = path.join(ROOT, 'reports', 'explainable-decisions.md');
  const explain = `# Explainable Decisions

- Constitution checker: ${constitution.constitutionalOk ? 'pass' : 'fail'}
- Policy checker violations: ${(policy.violations || []).length}
- Case-law overrides: ${(caseLaw.allowedByPrecedent || []).length}
- Appeals accepted: ${(appeals.accepted || []).length}
- Unmet policy violations: ${(caseLaw.unmet || []).length}
- Final verdict: ${verdict.verdict}
`;
  fs.writeFileSync(explainFile, explain);

  if (verdict.verdict === 'DENY' || verdict.verdict === 'UNCONSTITUTIONAL') {
    console.error(`FAIL court-engine verdict=${verdict.verdict}`);
    process.exit(1);
  }
  console.log(`PASS court-engine verdict=${verdict.verdict}`);
}

if (require.main === module) run();

module.exports = { run };
