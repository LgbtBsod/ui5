#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

const CASE_DIR = path.resolve(__dirname, '..', 'law', 'cases');

function parseCase(file) {
  const t = fs.readFileSync(file, 'utf8');
  const get = (k) => {
    const m = t.match(new RegExp(`^${k}:\\s*(.+)$`, 'm'));
    return m ? m[1].trim() : '';
  };
  return {
    case_id: get('case_id'),
    rule: get('rule'),
    decision: get('decision'),
    reason: get('reason'),
    expires_at: get('expires_at'),
    conditions: t.split('\n').filter((l) => l.trim().startsWith('- ')).map((l) => l.trim().slice(2))
  };
}

function checkCaseLaw(policyViolations) {
  if (!fs.existsSync(CASE_DIR)) return { allowedByPrecedent: [], unmet: [] };
  const cases = fs.readdirSync(CASE_DIR).filter((f) => f.endsWith('.yml')).map((f) => parseCase(path.join(CASE_DIR, f)));
  const now = new Date();
  const allowedByPrecedent = [];
  const unmet = [];

  for (const v of policyViolations || []) {
    const c = cases.find((x) => x.rule === v.rule && x.decision === 'allowed' && (!x.expires_at || new Date(x.expires_at) >= now));
    if (c) allowedByPrecedent.push({ violation: v, case: c });
    else unmet.push(v);
  }

  return { allowedByPrecedent, unmet };
}

module.exports = { checkCaseLaw };
