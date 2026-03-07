#!/usr/bin/env node

const { runPatternGate } = require('./lib/patternGateRuntime');

runPatternGate('manager-purity-gate', [
  { id: 'manager-model-write', regex: /\.setProperty\(|setData\(/, message: 'manager must not write model directly' }
], { include: ['service/runtime/**/*.js'] });
