const path = require('path');
const { loadSapModule } = require('../sap-module-loader');

function createResult(name, ok, detail) {
  return [{ name, ok, detail }];
}

async function runFrontendVariablesSmoke() {
  const cwd = process.cwd();
  const appRoot = path.basename(cwd) === 'app' ? cwd : path.join(cwd, 'app');
  const schemaPath = path.join(appRoot, 'util/runtime/FrontendVariablesSchema.js');

  const FrontendVariablesSchema = loadSapModule(schemaPath, {});
  const defaults = FrontendVariablesSchema.buildDefaults();

  const sanitized = FrontendVariablesSchema.sanitize({
    ENABLE_CLIENT_LOGS: 'true',
    MAX_SELECTION_SIZE: 'not-a-number',
    DEFAULT_EXPORT_FORMAT: 123,
    UNKNOWN_FLAG: 'drop-me'
  });

  const unknownDropped = !Object.prototype.hasOwnProperty.call(sanitized, 'UNKNOWN_FLAG');
  const booleansCoerced = sanitized.ENABLE_CLIENT_LOGS === true;
  const numberFallback = sanitized.MAX_SELECTION_SIZE === defaults.MAX_SELECTION_SIZE;
  const stringCoerced = sanitized.DEFAULT_EXPORT_FORMAT === '123';

  if (!unknownDropped || !booleansCoerced || !numberFallback || !stringCoerced) {
    return createResult(
      'runtime-frontend-variables',
      false,
      'frontend variables sanitizer must drop unknown keys and coerce known keys with defaults'
    );
  }

  return createResult(
    'runtime-frontend-variables',
    true,
    'frontend variables sanitizer gate passed'
  );
}

module.exports = { runFrontendVariablesSmoke };
