#!/usr/bin/env node

const fs = require('fs');
const vm = require('vm');

function buildDefaultSandbox(overrides) {
  return Object.assign({
    fetch,
    URL,
    Promise,
    window: {
      location: { href: 'http://localhost/?backend=fake' }
    }
  }, overrides || {});
}

function loadSapModule(modulePath, depMap, sandboxOverrides) {
  const code = fs.readFileSync(modulePath, 'utf8');
  let exported;

  const sandbox = buildDefaultSandbox(sandboxOverrides);
  sandbox.sap = sandbox.sap || {};
  sandbox.sap.ui = sandbox.sap.ui || {};
  sandbox.sap.ui.define = (deps, factory) => {
    if (typeof deps === 'function') {
      exported = deps();
      return;
    }

    const resolved = (deps || []).map((dep) => {
      if (!depMap || !(dep in depMap)) {
        throw new Error(`Missing dependency: ${dep} for ${modulePath}`);
      }
      return depMap[dep];
    });

    exported = factory(...resolved);
  };

  vm.createContext(sandbox);
  vm.runInContext(code, sandbox, { filename: modulePath });
  return exported;
}

module.exports = {
  loadSapModule
};
