#!/usr/bin/env node

const { runGate } = require('./lib/gateRunner');

const ALLOW = new Set([
  'scripts/qa-all.js',
  'scripts/network-contract-verifier.js',
  'Component.js',
  'service/framework/ComponentInitRuntime.js',
  'controller/support/DetailViewSupport.js',
  'controller/support/DetailControllerActions.js',
  'controller/support/AppControllerActions.js',
  'controller/support/SearchControllerActions.js',
  'controller/support/SearchViewSupport.js',
  'controller/support/DetailActionPinnedRailSupport.js',
  'infra/adapters/ODataChecklistRepoAdapter.js',
  'service/backend/GatewayClient.js',
  'service/domain/detail/usecases/SaveDetailUseCase.js',
  'scripts/lib/networkContractVerifierCore.js'
]);
const MAX = 200;

runGate({
  name: 'js-line-limit-gate',
  advisory: true,
  include: [
    'controller/**/*.js', 'service/**/*.js', 'infra/**/*.js', 'ports/**/*.js',
    'manager/**/*.js', 'model/**/*.js', 'util/**/*.js', 'scripts/**/*.js', 'Component.js'
  ],
  exclude: ['scripts/internal/**'],
  check: ({ file, lines }) => {
    if (ALLOW.has(file)) return [];
    if (lines <= MAX) return [];
    return [{ file, line: null, message: `${lines} lines exceeds ${MAX}` }];
  }
});
