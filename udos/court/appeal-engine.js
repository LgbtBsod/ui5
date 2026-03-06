#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

const APPEAL_DIR = path.resolve(__dirname, 'appeals');

function loadAppeals() {
  if (!fs.existsSync(APPEAL_DIR)) return [];
  return fs.readdirSync(APPEAL_DIR)
    .filter((f) => f.endsWith('.yml') || f.endsWith('.yaml'))
    .map((f) => fs.readFileSync(path.join(APPEAL_DIR, f), 'utf8'));
}

function parseAppeal(raw) {
  const get = (k) => {
    const m = raw.match(new RegExp(`^${k}:\\s*(.+)$`, 'm'));
    return m ? m[1].trim() : '';
  };
  return {
    appeal_id: get('appeal_id'),
    rule: get('rule'),
    reason: get('reason'),
    proposed_conditions: raw.split('\n').filter((l) => l.trim().startsWith('- ')).map((l) => l.trim().slice(2))
  };
}

function checkAppeals(unmetViolations) {
  const appeals = loadAppeals().map(parseAppeal);
  const accepted = [];
  const rejected = [];

  for (const v of unmetViolations || []) {
    const a = appeals.find((x) => x.rule === v.rule);
    if (a) accepted.push({ violation: v, appeal: a });
    else rejected.push(v);
  }
  return { accepted, rejected };
}

module.exports = { checkAppeals };
