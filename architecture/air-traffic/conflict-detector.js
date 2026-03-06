#!/usr/bin/env node

function intersect(a, b) {
  const setB = new Set(b || []);
  return [...new Set((a || []).filter((x) => setB.has(x)))];
}

function detectConflicts(queue) {
  const out = [];
  for (let i = 0; i < queue.length; i += 1) {
    for (let j = i + 1; j < queue.length; j += 1) {
      const a = queue[i];
      const b = queue[j];
      const fileOverlap = intersect(a.files, b.files);
      const moduleOverlap = intersect(
        (a.files || []).map((f) => String(f).split('/').slice(0, 3).join('/')),
        (b.files || []).map((f) => String(f).split('/').slice(0, 3).join('/'))
      );
      const workflowOverlap = intersect(a.workflows, b.workflows).concat(
        intersect(a.files.filter((f) => /enteredit|autosave|lock/i.test(f)), b.files.filter((f) => /enteredit|autosave|lock/i.test(f)))
      );
      const stateOverlap = intersect(a.statePaths, b.statePaths).filter((p) => ['/mode', '/lockOperationState'].includes(p));

      if (fileOverlap.length || moduleOverlap.length || workflowOverlap.length || stateOverlap.length) {
        out.push({
          pair: [a.flightPlanId, b.flightPlanId],
          fileConflicts: fileOverlap,
          moduleConflicts: moduleOverlap,
          workflowConflicts: [...new Set(workflowOverlap)],
          stateConflicts: stateOverlap
        });
      }
    }
  }
  return out;
}

module.exports = { detectConflicts };
