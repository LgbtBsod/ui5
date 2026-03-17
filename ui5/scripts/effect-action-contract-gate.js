#!/usr/bin/env node

const { runPatternGate } = require('./lib/patternGateRuntime');

runPatternGate('effect-action-contract-gate', [
  { id: 'legacy-action-yes', regex: /\bactionYes\b/, message: 'Legacy actionYes key is forbidden. Use confirmAction.' },
  { id: 'legacy-action-no', regex: /\bactionNo\b/, message: 'Legacy actionNo key is forbidden. Use cancelAction.' },
  { id: 'legacy-dispatch-action', regex: /\bdispatchAction\b/, message: 'Legacy dispatchAction key is forbidden. Use actionName.' }
]);
