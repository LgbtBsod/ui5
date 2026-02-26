#!/usr/bin/env node
const fs = require('fs');
const vm = require('vm');

function loadSapModule(path, depMap = {}) {
  const code = fs.readFileSync(path, 'utf8');
  let exported;
  const sandbox = {
    sap: {
      ui: {
        define: (deps, factory) => {
          if (typeof deps === 'function') {
            exported = deps();
            return;
          }
          const resolved = (deps || []).map((dep) => {
            if (!(dep in depMap)) {
              throw new Error(`Missing dependency: ${dep} for ${path}`);
            }
            return depMap[dep];
          });
          exported = factory(...resolved);
        }
      }
    },
    fetch,
    URL,
    URLSearchParams,
    Promise,
    console,
    window: {
      location: { href: 'http://localhost/?backend=fake' }
    }
  };
  vm.createContext(sandbox);
  vm.runInContext(code, sandbox, { filename: path });
  return exported;
}

function assert(cond, msg) {
  if (!cond) {
    throw new Error(msg);
  }
}

function validateCapabilities(capabilities, fixtures, mode) {
  const schema = fixtures.schema || {};
  const requiredRootKeys = schema.requiredRootKeys || [];
  const requiredCompatibilityKeys = schema.requiredCompatibilityKeys || [];
  const requiredFeatures = fixtures.requiredFeatures || [];

  requiredRootKeys.forEach((key) => {
    assert(Object.prototype.hasOwnProperty.call(capabilities, key), `${mode}: missing root key ${key}`);
  });

  requiredCompatibilityKeys.forEach((key) => {
    assert(capabilities.compatibility && capabilities.compatibility[key], `${mode}: missing compatibility key ${key}`);
  });

  requiredFeatures.forEach((feature) => {
    assert(capabilities.features && capabilities.features[feature] === true, `${mode}: required feature disabled ${feature}`);
  });

  const expectedMode = fixtures.modeExpectations && fixtures.modeExpectations[mode] && fixtures.modeExpectations[mode].backendMode;
  if (expectedMode) {
    assert(capabilities.backendMode === expectedMode, `${mode}: backendMode mismatch (${capabilities.backendMode} !== ${expectedMode})`);
  }
}

async function main() {
  const fixturesPath = process.argv[2] || 'scripts/backend-capability-fixtures.json';
  if (!fs.existsSync(fixturesPath)) {
    throw new Error(`Fixtures not found: ${fixturesPath}`);
  }
  const fixtures = JSON.parse(fs.readFileSync(fixturesPath, 'utf8'));

  const fakeBackend = loadSapModule('service/backend/FakeBackendService.js', {
    'sap_ui5/service/backend/InMemoryDB': {},
    'sap_ui5/service/backend/FakeODataService': {}
  });

  const realBackend = loadSapModule('service/backend/RealBackendService.js');

  const adapter = loadSapModule('service/backend/BackendAdapter.js', {
    'sap_ui5/service/backend/FakeBackendService': fakeBackend,
    'sap_ui5/service/backend/RealBackendService': realBackend
  });

  assert(typeof adapter.getCapabilities === 'function', 'BackendAdapter.getCapabilities should exist');
  assert(typeof adapter.negotiateCapabilities === 'function', 'BackendAdapter.negotiateCapabilities should exist');
  assert(typeof adapter.ensureContractCompatibility === 'function', 'BackendAdapter.ensureContractCompatibility should exist');

  const fakeCaps = await fakeBackend.getCapabilities();
  validateCapabilities(fakeCaps, fixtures, 'fake');

  const realCaps = await realBackend.getCapabilities();
  validateCapabilities(realCaps, fixtures, 'real');

  adapter.configure({ mode: 'fake' });
  const fakeNegotiation = await adapter.negotiateCapabilities(fixtures.requiredFeatures || []);
  assert(fakeNegotiation.ok === true, 'fake negotiation should pass required features');

  const sUiContractVersion = String(fixtures.uiContractVersion || '1.0.0');

  adapter.configure({ mode: 'fake' });
  const fakeCompatibility = await adapter.ensureContractCompatibility(sUiContractVersion);
  assert(fakeCompatibility.ok === true, 'fake contract compatibility should pass for baseline ui version');

  adapter.configure({ mode: 'real' });
  const realCompatibility = await adapter.ensureContractCompatibility(sUiContractVersion);
  assert(realCompatibility.ok === true, 'real contract compatibility should pass for baseline ui version');

  const unsupportedVersions = Array.isArray(fixtures.unsupportedUiContractVersions)
    ? fixtures.unsupportedUiContractVersions
    : [];
  for (const sVersion of unsupportedVersions) {
    adapter.configure({ mode: 'real' });
    const oCompat = await adapter.ensureContractCompatibility(sVersion);
    assert(oCompat.ok === false,
      `real contract compatibility should fail for unsupported ui version ${sVersion}`);
  }


  const malformed = fixtures.malformedSemverCompatibility || null;
  if (malformed) {
    const originalGetCapabilities = fakeBackend.getCapabilities;
    fakeBackend.getCapabilities = () => Promise.resolve({
      contractVersion: '1.0.0',
      backendMode: 'fake',
      features: fakeCaps.features,
      compatibility: {
        minUiContractVersion: String(malformed.minUiContractVersion || ''),
        maxUiContractVersion: String(malformed.maxUiContractVersion || '')
      },
      source: 'fixture_malformed_semver'
    });

    adapter.configure({ mode: 'fake' });
    const malformedCompat = await adapter.ensureContractCompatibility(String(malformed.uiContractVersion || '1.0.0'));
    assert(malformedCompat.ok === false && malformedCompat.reason === 'invalid_semver_metadata',
      'malformed semver compatibility metadata should be rejected');

    fakeBackend.getCapabilities = originalGetCapabilities;
  }

  const exactBound = fixtures.exactUpperBoundCompatibility || null;
  if (exactBound) {
    const originalGetCapabilities = fakeBackend.getCapabilities;
    fakeBackend.getCapabilities = () => Promise.resolve({
      contractVersion: '1.0.0',
      backendMode: 'fake',
      features: fakeCaps.features,
      compatibility: {
        minUiContractVersion: String(exactBound.minUiContractVersion || '1.0.0'),
        maxUiContractVersion: String(exactBound.maxUiContractVersion || '1.0.0')
      },
      source: 'fixture_exact_upper_bound'
    });

    adapter.configure({ mode: 'fake' });
    const exactOk = await adapter.ensureContractCompatibility(String(exactBound.supportedUiContractVersion || '1.0.0'));
    assert(exactOk.ok === true, 'exact upper bound should include supportedUiContractVersion');

    const exactBad = await adapter.ensureContractCompatibility(String(exactBound.unsupportedUiContractVersion || '9.9.9'));
    assert(exactBad.ok === false && exactBad.reason === 'ui_contract_out_of_supported_range',
      'exact upper bound should reject unsupportedUiContractVersion');

    fakeBackend.getCapabilities = originalGetCapabilities;
  }

  console.log('Backend contract drift gate passed.');
}

main().catch((err) => {
  console.error(`Backend contract drift gate failed: ${err.message}`);
  process.exit(1);
});
