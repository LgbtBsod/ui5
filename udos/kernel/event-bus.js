#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const cfg = require('./config');

const ALLOWED = new Set([
  'change_detected',
  'flight_plan_generated',
  'preflight_completed',
  'clearance_granted',
  'postflight_completed',
  'mission_created',
  'budget_exceeded',
  'policy_violation',
  'score_updated',
  'duplication_detected',
  'rewrite_proved'
]);

function ensureDir(file) {
  const dir = path.dirname(file);
  if (!fs.existsSync(dir)) fs.mkdirSync(dir, { recursive: true });
}

function emit(type, payload) {
  if (!ALLOWED.has(type)) throw new Error(`Unknown UDOS event: ${type}`);
  const e = { ts: new Date().toISOString(), type, payload: payload || {} };
  ensureDir(cfg.eventsFile);
  fs.appendFileSync(cfg.eventsFile, JSON.stringify(e) + '\n');
  return e;
}

module.exports = { emit, ALLOWED: [...ALLOWED] };
