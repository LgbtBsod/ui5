#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const { loadOpenFlights, buildQueue } = require('./queue-manager');
const { detectConflicts } = require('./conflict-detector');
const { schedule } = require('./scheduler');
const { coordinate } = require('./merge-coordinator');
const { detectDeadlocks } = require('./deadlock-detector');

const ROOT = path.resolve(__dirname, '..', '..');
const DASHBOARD = path.join(ROOT, 'docs', 'air-traffic', 'traffic-dashboard.md');
const REPORT = path.join(ROOT, 'docs', 'air-traffic', 'traffic-report.md');

function ensureDir(p) { if (!fs.existsSync(p)) fs.mkdirSync(p, { recursive: true }); }

function metrics(openFlights, conflicts, twinSummary) {
  const flightsPerWeek = openFlights.length;
  const deniedFlights = openFlights.filter((f) => !f.hasPreflight || !f.flightPlanId).length;
  const conflictRate = openFlights.length ? Number((conflicts.length / openFlights.length).toFixed(2)) : 0;
  const scoreMatch = String(twinSummary || '').match(/ADT_SCORE:\s*\*\*(\d+(?:\.\d+)?)\*\*/);
  const architectureScoreTrend = scoreMatch ? Number(scoreMatch[1]) : null;
  return { flightsPerWeek, deniedFlights, conflictRate, architectureScoreTrend };
}

function run() {
  const open = loadOpenFlights();
  const queue = buildQueue(open);
  const conflicts = detectConflicts(queue);
  const scheduled = schedule(queue, conflicts);
  const deadlocks = detectDeadlocks(conflicts);
  const mergeState = coordinate(queue);

  const twinSummary = fs.existsSync(path.join(ROOT, 'docs', 'digital-twin', 'twin-summary.md'))
    ? fs.readFileSync(path.join(ROOT, 'docs', 'digital-twin', 'twin-summary.md'), 'utf8')
    : '';
  const m = metrics(queue, conflicts, twinSummary);

  const dashboard = `# Architecture Air Traffic Dashboard\n\n## Open Flights\n${queue.map((f) => `- ${f.flightPlanId} (PR #${f.prNumber || '-'}, risk=${f.risk})`).join('\n') || '- (none)'}\n\n## Queue order\n${queue.map((f, i) => `${i + 1}. ${f.flightPlanId}`).join('\n') || '-'}\n\n## Conflicts detected\n${conflicts.map((c) => `- ${c.pair.join(' ↔ ')} | files=${c.fileConflicts.length}, workflows=${c.workflowConflicts.length}, state=${c.stateConflicts.join(',') || '-'}`).join('\n') || '- none'}\n\n## Scheduled merges\n${scheduled.map((s) => `- ${s.mode}: ${s.flights.join(', ')}`).join('\n') || '- none'}\n\n## Exclusive windows\n${scheduled.filter((s) => s.mode === 'exclusive').map((s) => `- ${s.flights[0]}`).join('\n') || '- none'}\n`;

  const report = `# Architecture Air Traffic Report\n\n## Queue state\n- flights: ${queue.length}\n- next: ${(mergeState && mergeState.next) || '-'}\n\n## Conflicts\n- total conflict pairs: ${conflicts.length}\n- deadlocks: ${deadlocks.length}\n${deadlocks.map((d) => `- ${d.pair.join(' <-> ')} => ${d.resolution}`).join('\n') || ''}\n\n## Scheduling decisions\n${scheduled.map((s) => `- ${s.mode}: ${s.flights.join(', ')}${s.reason ? ` (${s.reason})` : ''}`).join('\n') || '- none'}\n\n## Architecture impact\n- Flights per week: ${m.flightsPerWeek}\n- Denied flights: ${m.deniedFlights}\n- Conflict rate: ${m.conflictRate}\n- Architecture score trend (ADT): ${m.architectureScoreTrend == null ? '-' : m.architectureScoreTrend}\n\n## Merge coordinator clearance\n- ${(mergeState.evaluation && mergeState.evaluation.flightPlanId) || '-'}: ${(mergeState.evaluation && mergeState.evaluation.action) || '-'}\n`;

  ensureDir(path.dirname(DASHBOARD));
  fs.writeFileSync(DASHBOARD, dashboard);
  fs.writeFileSync(REPORT, report);

  const rejectNoPlan = queue.some((f) => !f.flightPlanId);
  const rejectNoPreflight = queue.some((f) => !f.hasPreflight);
  if (rejectNoPlan || rejectNoPreflight) {
    console.error('FAIL air-traffic-control: missing flight plan or preflight evidence');
    process.exit(1);
  }
  console.log('PASS air-traffic-control');
}

if (require.main === module) run();

module.exports = { run, metrics };
