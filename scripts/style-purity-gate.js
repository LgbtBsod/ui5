#!/usr/bin/env node

const { runPatternGate } = require('./lib/patternGateRuntime');

runPatternGate(
  'style-purity-gate',
  [{ id: 'style-mutation', regex: /addStyleClass\(|removeStyleClass\(/, message: 'Style mutation allowed only in Ui5StyleAdapter' }],
  { shouldSkip: function (file) { return file === 'infra/adapters/Ui5StyleAdapter.js'; } }
);
