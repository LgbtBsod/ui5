#!/usr/bin/env node

function detectDeadlocks(conflicts) {
  const waits = new Map();
  (conflicts || []).forEach((c) => {
    const [a, b] = c.pair;
    if (!waits.has(a)) waits.set(a, new Set());
    if (!waits.has(b)) waits.set(b, new Set());
    waits.get(a).add(b);
    waits.get(b).add(a);
  });

  const deadlocks = [];
  for (const [a, deps] of waits.entries()) {
    for (const b of deps) {
      if (waits.has(b) && waits.get(b).has(a) && a < b) {
        deadlocks.push({
          pair: [a, b],
          resolution: `Merge ${a} first with reduced scope, then rebase ${b}`
        });
      }
    }
  }
  return deadlocks;
}

module.exports = { detectDeadlocks };
