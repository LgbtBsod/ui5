#!/usr/bin/env node

function buildSchedule() {
  return {
    daily: ['telemetry'],
    weekly: ['debt-forecast'],
    nightly: ['duplication-scan'],
    exclusiveWindows: ['HIGH-risk flights (lock/autosave/cache)']
  };
}

module.exports = { buildSchedule };
