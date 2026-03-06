#!/usr/bin/env node

function schedule(queue, conflicts) {
  const conflictMap = new Map();
  (conflicts || []).forEach((c) => {
    const [a, b] = c.pair;
    conflictMap.set(`${a}::${b}`, c);
    conflictMap.set(`${b}::${a}`, c);
  });

  const plan = [];
  let parallelLane = [];
  let exclusiveActive = false;

  for (const f of queue) {
    const risk = String(f.risk || 'MED').toUpperCase();
    if (risk === 'HIGH') {
      if (parallelLane.length) {
        plan.push({ mode: 'parallel', flights: parallelLane.slice() });
        parallelLane = [];
      }
      plan.push({ mode: 'exclusive', flights: [f.flightPlanId], reason: 'HIGH-risk exclusive window' });
      exclusiveActive = true;
      continue;
    }

    if (risk === 'MED') {
      if (parallelLane.length) {
        plan.push({ mode: 'parallel', flights: parallelLane.slice() });
        parallelLane = [];
      }
      plan.push({ mode: 'sequential', flights: [f.flightPlanId], reason: 'MED-risk sequential merge' });
      continue;
    }

    const hasLaneConflict = parallelLane.some((id) => conflictMap.has(`${id}::${f.flightPlanId}`));
    if (exclusiveActive || hasLaneConflict) {
      if (parallelLane.length) {
        plan.push({ mode: 'parallel', flights: parallelLane.slice() });
        parallelLane = [];
      }
      plan.push({ mode: 'sequential', flights: [f.flightPlanId], reason: hasLaneConflict ? 'conflict in parallel lane' : 'post-exclusive sequencing' });
      exclusiveActive = false;
    } else {
      parallelLane.push(f.flightPlanId);
    }
  }
  if (parallelLane.length) {
    plan.push({ mode: 'parallel', flights: parallelLane.slice() });
  }
  return plan;
}

module.exports = { schedule };
