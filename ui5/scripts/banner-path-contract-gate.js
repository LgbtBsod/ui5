#!/usr/bin/env node

const { runGate } = require('./lib/gateRunner');
const { scanPatterns } = require('./lib/patternScan');

const ALLOW = new Set([
  'service/domain/shared/StatePaths.js'
]);

runGate({
  name: 'banner-path-contract-gate',
  include: ['controller/**/*.js', 'service/**/*.js', 'infra/**/*.js', 'util/**/*.js', 'model/**/*.js', 'ports/**/*.js'],
  check: ({ file, text }) => {
    if (ALLOW.has(file)) {
      return [];
    }
    return scanPatterns(file, text, [
      { id: 'banner-global-path-literal', regex: /\/ui\/feedback\/banner\/global/g, message: 'Use FeedbackBannerRuntime or StatePaths constant instead of hardcoded banner path.' }
    ]);
  }
});
