#!/usr/bin/env node
const fs = require('fs');

const path = 'docs/artifacts/control-token-mapping.json';
if (!fs.existsSync(path)) {
  console.error('Control-token mapping gate failed: artifact missing.');
  process.exit(1);
}

const data = JSON.parse(fs.readFileSync(path, 'utf8'));
const requiredFamilies = ['shellLayout','primaryInputs','dataSurfaces','overlays','smartControls','advancedTables','fioriCards','runtimeModules'];
const requiredRoles = ['radius','glass-bg','glass-border','focus-ring','motion','opacity'];

if (String(data.ui5Baseline || '').trim() !== '1.71.28') {
  console.error('Control-token mapping gate failed: ui5Baseline must be 1.71.28');
  process.exit(1);
}

const families = data.families || {};
for (const fam of requiredFamilies) {
  if (!families[fam]) {
    console.error(`Control-token mapping gate failed: missing family ${fam}`);
    process.exit(1);
  }
  const controls = families[fam].controls || [];
  const roles = families[fam].roles || [];
  if (!controls.length || !roles.length) {
    console.error(`Control-token mapping gate failed: family ${fam} has empty controls/roles`);
    process.exit(1);
  }
}

const allRoles = new Set(Object.values(families).flatMap((f) => f.roles || []));
const missingRoles = requiredRoles.filter((r) => !allRoles.has(r));
if (missingRoles.length) {
  console.error(`Control-token mapping gate failed: missing token role coverage: ${missingRoles.join(', ')}`);
  process.exit(1);
}

console.log('Control-token mapping gate passed: all required control families and token roles are covered.');
