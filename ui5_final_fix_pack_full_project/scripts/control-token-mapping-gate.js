#!/usr/bin/env node

const { runJsonContractGate } = require('./lib/artifactContractGate');

const requiredFamilies = ['shellLayout', 'primaryInputs', 'dataSurfaces', 'overlays', 'smartControls', 'advancedTables', 'fioriCards', 'runtimeModules'];
const requiredRoles = ['radius', 'glass-bg', 'glass-border', 'focus-ring', 'motion', 'opacity'];

runJsonContractGate({
  dataPath: 'docs/artifacts/control-token-mapping.json',
  missingMessage: 'Control-token mapping gate failed: artifact missing.',
  passMessage: 'Control-token mapping gate passed: all required control families and token roles are covered.',
  validate: function (data) {
    const issues = [];
    const families = data && data.families ? data.families : {};
    if (String((data && data.ui5Baseline) || '').trim() !== '1.71.28') {
      issues.push('Control-token mapping gate failed: ui5Baseline must be 1.71.28');
    }
    requiredFamilies.forEach(function (family) {
      if (!families[family]) {
        issues.push(`Control-token mapping gate failed: missing family ${family}`);
        return;
      }
      const controls = families[family].controls || [];
      const roles = families[family].roles || [];
      if (!controls.length || !roles.length) {
        issues.push(`Control-token mapping gate failed: family ${family} has empty controls/roles`);
      }
    });
    const allRoles = new Set(Object.values(families).flatMap(function (family) { return family.roles || []; }));
    const missingRoles = requiredRoles.filter(function (role) { return !allRoles.has(role); });
    if (missingRoles.length) {
      issues.push(`Control-token mapping gate failed: missing token role coverage: ${missingRoles.join(', ')}`);
    }
    return issues;
  }
});
