#!/usr/bin/env node

const { runGate } = require('./lib/gateRunner');
const { scanPatterns } = require('./lib/patternScan');

runGate({
  name: 'action-dispatcher-contract-gate',
  include: ['controller/**/*.js', 'service/**/*.js', 'infra/**/*.js', 'manager/**/*.js', 'util/**/*.js', 'model/**/*.js', 'ports/**/*.js', 'facades/**/*.js'],
  check: ({ file, text }) => {
    return scanPatterns(file, text, [
      { id: 'register-map-call', regex: /\.registerMap\s*\(/g, message: 'ActionDispatcher.registerMap is deprecated. Use register(action, handler).' },
      { id: 'register-map-api', regex: /registerMap\s*:\s*function|registerMap\s*=\s*function|prototype\.registerMap\s*=/g, message: 'ActionDispatcher.registerMap API is forbidden.' }
    ]);
  }
});
