#!/usr/bin/env node

function buildVerdict({ constitution, policy, caseLaw, appeals }) {
  if (!constitution.constitutionalOk) {
    return { verdict: 'UNCONSTITUTIONAL', conditions: [], reasons: constitution.violations };
  }

  if (policy.policyOk) {
    return { verdict: 'CLEAR', conditions: [], reasons: [] };
  }

  if ((caseLaw.unmet || []).length === 0) {
    return {
      verdict: 'ALLOWED BY PRECEDENT',
      conditions: caseLaw.allowedByPrecedent.flatMap((x) => x.case.conditions || []),
      reasons: ['Policy violations covered by active precedents']
    };
  }

  if ((appeals.accepted || []).length > 0 && (appeals.rejected || []).length === 0) {
    return {
      verdict: 'CLEAR WITH CONDITIONS',
      conditions: appeals.accepted.flatMap((x) => x.appeal.proposed_conditions || []),
      reasons: ['Appeal accepted']
    };
  }

  return {
    verdict: 'DENY',
    conditions: [],
    reasons: (appeals.rejected || []).map((v) => v.msg || v.rule)
  };
}

module.exports = { buildVerdict };
