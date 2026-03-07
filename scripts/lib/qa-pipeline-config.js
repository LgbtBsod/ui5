const validators = [
  { name: 'forbidden-patterns', file: 'forbidden-patterns.js' },
  { name: 'gateway-parity-validator', file: 'gateway-parity-validator.js' },
  { name: 'sap-gateway-only-gate', file: 'sap-gateway-only-gate.js' },
  { name: 'smart-odata-contract-gate', file: 'smart-odata-contract-gate.js' },
  { name: 'architecture-gate', file: 'architecture-gate.js', mode: 'advisory' },
  { name: 'style-scan', file: 'style-scan.js', mode: 'advisory' },
  { name: 'feature-scan', file: 'feature-scan.js', mode: 'advisory' },
  { name: 'final-architecture-freeze-gate', file: 'ci/final-architecture-freeze-gate.js', mode: 'advisory' },
  { name: 'legacy-ban-gate', file: 'ci/legacy-ban-gate.js' },
  { name: 'shadow-duplicate-gate', file: 'ci/shadow-duplicate-gate.js', mode: 'advisory' },
  { name: 'runtime-duplication-gate', file: 'runtime-duplication-gate.js', mode: 'advisory' },
  { name: 'controller-purity-gate', file: 'controller-purity-gate.js', mode: 'advisory' },
  { name: 'runtime-settings-gate', file: 'runtime-settings-gate.js' },
  { name: 'function-length-gate', file: 'function-length-gate.js', mode: 'advisory' },
  { name: 'js-line-limit-gate', file: 'js-line-limit-gate.js', mode: 'advisory' },
  { name: 'layer-map', file: 'layer-map.js', mode: 'advisory' },
  { name: 'usecase-no-ui5-import-gate', file: 'usecase-no-ui5-import-gate.js' },
  { name: 'usecase-contract-gate', file: 'usecase-contract-gate.js' },
  { name: 'effects-whitelist-gate', file: 'effects-whitelist-gate.js' },
  { name: 'action-dispatcher-contract-gate', file: 'action-dispatcher-contract-gate.js' },
  { name: 'effect-action-contract-gate', file: 'effect-action-contract-gate.js' },
  { name: 'banner-path-contract-gate', file: 'banner-path-contract-gate.js' },
  { name: 'network-signature-fixture', file: 'testing/network-signature-fixture.js' },
  { name: 'suggest-on-interaction-only-gate', file: 'suggest-on-interaction-only-gate.js' },
  { name: 'network-contract-verifier', file: 'network-contract-verifier.js' },
  { name: 'backend-service-dedup-gate', file: 'backend-service-dedup-gate.js' },
  { name: 'dedup-fingerprint-gate', file: 'dedup-fingerprint-gate.js', mode: 'advisory' },
  { name: 'feedback-unification-gate', file: 'feedback-unification-gate.js' },
  { name: 'model-path-contract-gate', file: 'model-path-contract-gate.js' },
  { name: 'listener-registration-gate', file: 'ci/listener-registration-gate.js', mode: 'advisory' },
  { name: 'uiState-workflow-mirror-gate', file: 'ci/uiState-workflow-mirror-gate.js', mode: 'advisory' },
  { name: 'compat-entity-access-gate', file: 'compat-entity-access-gate.js' },
  { name: 'controller-import-whitelist-gate', file: 'controller-import-whitelist-gate.js' },
  { name: 'manager-purity-gate', file: 'manager-purity-gate.js' },
  { name: 'style-purity-gate', file: 'style-purity-gate.js', mode: 'advisory' },
  { name: 'design-language-gate', file: 'design-language-gate.js', mode: 'advisory' },
  { name: 'final-static-qa', file: 'final-static-qa.js' },
  { name: 'enterprise-readiness-gate', file: 'enterprise-readiness-gate.js', mode: 'advisory' },
  { name: 'smarttable-beforeRebind-noRebind-gate', file: 'ci/smarttable-beforeRebind-noRebind-gate.js' },
  { name: 'statepaths-schema-consistency-gate', file: 'ci/statepaths-schema-consistency-gate.js' },
  { name: 'edit-requires-lock-gate', file: 'ci/edit-requires-lock-gate.js' },
  { name: 'autosave-input-contract-gate', file: 'ci/autosave-input-contract-gate.js' },
  { name: 'dead-code-gate', file: 'ci/dead-code-gate.js', mode: 'advisory' },
  { name: 'dependency-drift-gate', file: 'ci/dependency-drift-gate.js', mode: 'advisory' }
];

function getOptionalValidators(env) {
  var mEnv = env || process.env || {};
  return [
    { name: 'metrics-runner', file: 'metrics-runner.js', enabled: mEnv.ENABLE_METRICS === '1' },
    { name: 'gateway-live-smoke', file: 'gateway-live-smoke-runner.js', enabled: mEnv.ENABLE_GATEWAY_SMOKE === '1' }
  ];
}

module.exports = {
  validators,
  getOptionalValidators
};
