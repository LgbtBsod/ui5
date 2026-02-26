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
    Promise,
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

async function main() {
  const fakeBackend = {
    configure: () => {},
    getCapabilities: () => Promise.resolve({
      contractVersion: '1.0.0',
      backendMode: 'fake',
      features: { lockStatus: true },
      compatibility: { minUiContractVersion: '1.3.0', maxUiContractVersion: '1.2.0' },
      source: 'invalid-policy-fake'
    }),
    init: () => Promise.resolve([])
  };

  const realBackend = {
    configure: () => {},
    getCapabilities: () => Promise.resolve({
      contractVersion: '1.0.0',
      backendMode: 'real',
      features: { lockStatus: true },
      compatibility: { minUiContractVersion: '1.0.0', maxUiContractVersion: '1.x' },
      source: 'real-policy-ok'
    }),
    init: () => Promise.resolve([])
  };

  const adapter = loadSapModule('service/backend/BackendAdapter.js', {
    'sap_ui5/service/backend/FakeBackendService': fakeBackend,
    'sap_ui5/service/backend/RealBackendService': realBackend
  });


  const malformedBackend = {
    configure: () => {},
    getCapabilities: () => Promise.resolve({
      contractVersion: '1.0.0',
      backendMode: 'fake',
      features: { lockStatus: true },
      compatibility: { minUiContractVersion: '1.a.0', maxUiContractVersion: '1.x' },
      source: 'malformed-policy-fake'
    }),
    init: () => Promise.resolve([])
  };

  const malformedAdapter = loadSapModule('service/backend/BackendAdapter.js', {
    'sap_ui5/service/backend/FakeBackendService': malformedBackend,
    'sap_ui5/service/backend/RealBackendService': realBackend
  });

  malformedAdapter.configure({ mode: 'fake' });
  const malformed = await malformedAdapter.ensureContractCompatibility('1.0.0');
  assert(malformed.ok === false && malformed.reason === 'invalid_semver_metadata',
    'malformed semver metadata should be rejected deterministically');

  adapter.configure({ mode: 'fake' });
  const invalidPolicy = await adapter.ensureContractCompatibility('1.2.0');
  assert(invalidPolicy.ok === false && invalidPolicy.reason === 'invalid_semver_policy_range',
    'invalid semver policy should be rejected');

  let failFastRejected = false;
  try {
    await adapter.enforceContractCompatibility('1.2.0');
  } catch (err) {
    failFastRejected = /invalid_semver_policy_range/.test(String(err && err.message || ''));
  }
  assert(failFastRejected, 'enforceContractCompatibility should fail-fast on invalid policy');

  adapter.configure({ mode: 'real', uiContractVersion: '2.0.0' });
  let initRejected = false;
  try {
    await adapter.init();
  } catch (err) {
    initRejected = /ui_contract_out_of_supported_range/.test(String(err && err.message || ''));
  }
  assert(initRejected, 'init should fail-fast for incompatible configured ui contract version');

  console.log('Backend semver policy gate passed.');
}

main().catch((err) => {
  console.error(`Backend semver policy gate failed: ${err.message}`);
  process.exit(1);
});
