#!/usr/bin/env node
const { execSync } = require('child_process');

function evaluateFlight(flight) {
  const hasPlan = !!flight.flightPlanId;
  const hasPreflight = !!flight.hasPreflight;

  let preflightOk = false;
  try {
    execSync('npm run digital-twin:preflight', { stdio: 'pipe' });
    preflightOk = true;
  } catch (_) {
    preflightOk = false;
  }

  const clearance = hasPlan && hasPreflight && preflightOk;
  return {
    flightPlanId: flight.flightPlanId,
    hasPlan,
    hasPreflight,
    preflightOk,
    clearance,
    action: clearance ? 'ALLOW_MERGE' : 'HOLD'
  };
}

function coordinate(queue) {
  if (!queue.length) return { next: null, status: 'NO_FLIGHTS' };
  const next = queue[0];
  return { next: next.flightPlanId, evaluation: evaluateFlight(next) };
}

module.exports = { coordinate, evaluateFlight };
