#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { readJsonSafe } = require('../../scripts/lib/auditInput');

const ROOT = path.resolve(__dirname, '..', '..');
const OPEN_FLIGHTS_PATH = path.join(ROOT, 'docs', 'air-traffic', 'open-flights.json');

function loadOpenFlights() {
  if (fs.existsSync(OPEN_FLIGHTS_PATH)) {
    return readJsonSafe(OPEN_FLIGHTS_PATH, []);
  }
  return [];
}

function normalizeFlight(raw, index) {
  const id = raw.flightPlanId || raw.id || `FP-${new Date().toISOString().slice(0, 10)}-${String(index + 1).padStart(3, '0')}`;
  return {
    flightPlanId: id,
    prNumber: raw.prNumber || null,
    title: raw.title || id,
    risk: String(raw.risk || 'MED').toUpperCase(),
    files: raw.files || [],
    statePaths: raw.statePaths || [],
    workflows: raw.workflows || [],
    hasPreflight: raw.hasPreflight !== false,
    openedAt: raw.openedAt || new Date().toISOString()
  };
}

function buildQueue(openFlights) {
  const normalized = openFlights.map(normalizeFlight);
  const priority = { HIGH: 0, MED: 1, LOW: 2 };
  return normalized.sort((a, b) => {
    const pa = priority[a.risk] ?? 9;
    const pb = priority[b.risk] ?? 9;
    if (pa !== pb) return pa - pb;
    return String(a.openedAt).localeCompare(String(b.openedAt));
  });
}

if (require.main === module) {
  const queue = buildQueue(loadOpenFlights());
  console.log(JSON.stringify({ queue }, null, 2));
}

module.exports = { loadOpenFlights, buildQueue, normalizeFlight };
