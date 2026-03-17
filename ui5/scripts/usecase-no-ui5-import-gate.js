#!/usr/bin/env node

const { runPatternGate } = require('./lib/patternGateRuntime');

runPatternGate('usecase-no-ui5-import-gate', [
  { id: 'ui5-import', regex: /['"]sap\//, message: 'Domain usecases must not import sap/* modules' }
], { include: ['service/domain/**/usecases/**/*.js'] });
