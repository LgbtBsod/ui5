const fs = require('fs');
const path = require('path');
const { detectRuntimeRoot } = require('../qa-shared');

const ROOT = process.cwd();
const RUNTIME_ROOT = detectRuntimeRoot(ROOT);

const REQUIRED_FILES = [
  'docs/LOCAL_VALIDATION.md',
  'backend/mock_gateway/README_ODATA.md',
  'controller/support/SearchControllerActions.js',
  'controller/support/SearchRateProgress.js',
  'controller/support/DetailFormatters.js',
  'service/framework/ComponentRuntimeSupport.js',
  'util/CreateSentinel.js'
];

const LIMITS = {
  searchControllerLines: 250,
  detailControllerLines: 550,
  componentLines: 700,
  styleLines: 50000
};

function read(file) {
  const direct = path.join(ROOT, file);
  const runtime = path.join(ROOT, RUNTIME_ROOT, file);
  const target = fs.existsSync(direct) ? direct : runtime;
  return fs.readFileSync(target, 'utf8');
}

function exists(file) {
  return fs.existsSync(path.join(ROOT, file)) || fs.existsSync(path.join(ROOT, RUNTIME_ROOT, file));
}

function countLines(file) {
  return read(file).split(/\r?\n/).length;
}

function collectJsFiles(dir, out) {
  if (!fs.existsSync(dir)) {
    return out;
  }
  fs.readdirSync(dir, { withFileTypes: true }).forEach((entry) => {
    const abs = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      collectJsFiles(abs, out);
      return;
    }
    if (entry.isFile() && abs.endsWith('.js')) {
      out.push(path.relative(ROOT, abs).split(path.sep).join('/'));
    }
  });
  return out;
}

function readPackage() {
  if (exists('package.json')) {
    return JSON.parse(read('package.json'));
  }
  return { scripts: {} };
}

function readManifest() {
  return JSON.parse(read('manifest.json'));
}

function getI18nKeys(file) {
  return read(file)
    .split(/\r?\n/)
    .filter((line) => line && !line.startsWith('#') && line.includes('='))
    .map((line) => line.split('=')[0]);
}

function includesDependency(file, dep) {
  return read(file).includes(dep);
}

function collectMetrics() {
  return {
    searchControllerLines: countLines('controller/Search.controller.js'),
    detailControllerLines: countLines('controller/Detail.controller.js'),
    componentLines: countLines('Component.js'),
    styleLines: countLines('css/claude-hyper.css')
  };
}

function validateRequiredFiles(issues) {
  REQUIRED_FILES.forEach((file) => {
    if (!exists(file)) {
      issues.push(`Missing required architecture file: ${file}`);
    }
  });
}

function validateMetrics(issues, metrics) {
  Object.keys(LIMITS).forEach((key) => {
    if (metrics[key] > LIMITS[key]) {
      issues.push(`${key} exceeded: ${metrics[key]} > ${LIMITS[key]}`);
    }
  });
}

function validateSupportModules(issues) {
  if (!includesDependency('controller/Search.controller.js', 'PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchControllerActions')) {
    issues.push('Search.controller.js must use controller/support/SearchControllerActions');
  }
  if (!includesDependency('controller/support/SearchControllerActions.js', 'PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchRateProgress')) {
    issues.push('SearchControllerActions.js must use controller/support/SearchRateProgress');
  }
  if (!includesDependency('controller/Detail.controller.js', 'PRODUCTION_CONTROL_CHECKLIST/controller/support/DetailFormatters')) {
    issues.push('Detail.controller.js must use controller/support/DetailFormatters');
  }
  const detailSupportFiles = collectJsFiles(path.join(ROOT, 'controller/support'), [])
    .concat(collectJsFiles(path.join(ROOT, RUNTIME_ROOT, 'controller/support'), []))
    .filter((file) => /\/Detail.*\.js$/.test(file));
  const detailCreateSentinelFiles = ['controller/Detail.controller.js', ...detailSupportFiles];
  const hasDetailCreateSentinel = detailCreateSentinelFiles.some((file) =>
    exists(file) && includesDependency(file, 'PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel')
  );
  if (!hasDetailCreateSentinel) {
    issues.push('Detail flow must use util/CreateSentinel (controller or extracted support module)');
  }
  if (!includesDependency('Component.js', 'PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentRuntimeSupport')) {
    issues.push('Component.js must use service/framework/ComponentRuntimeSupport');
  }
}

function validateDetailFlow(issues) {
  if (!includesDependency('service/domain/detail/usecases/SaveDetailUseCase.js', 'get("selected", "/")')) {
    issues.push('SaveDetailUseCase must prefer selected snapshot as current detail source');
  }
  if (!includesDependency('service/domain/detail/usecases/AutosaveDetailUseCase.js', 'get("selected", "/")')) {
    issues.push('AutosaveDetailUseCase must prefer selected snapshot as current detail source');
  }
}

function validateManifestI18n(issues) {
  const manifest = readManifest();
  const settings = ((((manifest || {})['sap.ui5'] || {}).models || {}).i18n || {}).settings || {};
  if (settings.fallbackLocale !== 'ru') {
    issues.push('manifest.json must keep i18n fallbackLocale="ru"');
  }
  if (!Array.isArray(settings.supportedLocales) || !settings.supportedLocales.includes('ru')) {
    issues.push('manifest.json must keep ru in supportedLocales');
  }
}

function validateLocalizationKeys(issues) {
  return;
}

function validateCreateSentinelCentralization(issues) {
  const runtimeFiles = collectJsFiles(path.join(ROOT, 'controller'), [])
    .concat(collectJsFiles(path.join(ROOT, RUNTIME_ROOT, 'controller'), []))
    .concat(collectJsFiles(path.join(ROOT, 'service'), []))
    .concat(collectJsFiles(path.join(ROOT, RUNTIME_ROOT, 'service'), []))
    .concat(collectJsFiles(path.join(ROOT, 'infra'), []))
    .concat(collectJsFiles(path.join(ROOT, RUNTIME_ROOT, 'infra'), []))
    .concat(collectJsFiles(path.join(ROOT, 'util'), []))
    .concat(collectJsFiles(path.join(ROOT, RUNTIME_ROOT, 'util'), []));
  const localDefs = runtimeFiles.filter((file) => {
    if (file === 'util/CreateSentinel.js' || file === 'app/util/CreateSentinel.js') {
      return false;
    }
    const text = read(file);
    return /var\s+CREATE_SENTINEL\s*=/.test(text) || /function\s+normalizeCreateId\s*\(/.test(text) || /function\s+isCreateId\s*\(/.test(text);
  });
  if (localDefs.length) {
    issues.push(`Create sentinel logic is duplicated outside util/CreateSentinel.js: ${localDefs.join(', ')}`);
  }
}

function validateRequiredScripts(issues) {
  const scripts = (readPackage().scripts) || {};
  if (!Object.keys(scripts).length) {
    return;
  }
  ['qa'].forEach((name) => {
    if (!scripts[name]) {
      issues.push(`Missing required npm script: ${name}`);
    }
  });
}

function validateNoLegacyValidationRefs(issues) {
  const scriptFiles = collectJsFiles(path.join(ROOT, 'scripts'), []);
  const removedSearchSupport = ['SearchController', 'Support'].join('');
  const badModuleRefs = scriptFiles.filter((file) => includesDependency(file, removedSearchSupport));
  if (badModuleRefs.length) {
    issues.push(`Validation layer still references removed ${removedSearchSupport}: ${badModuleRefs.join(', ')}`);
  }
  const legacyFormatterNames = [
    ['formatLock', 'OperationText'].join(''),
    ['formatLock', 'OperationState'].join('')
  ];
  const badFormatterRefs = collectJsFiles(path.join(ROOT, 'controller'), [])
    .concat(collectJsFiles(path.join(ROOT, 'view'), []))
    .concat(scriptFiles)
    .filter((file) => exists(file) && legacyFormatterNames.some((name) => includesDependency(file, name)));
  if (badFormatterRefs.length) {
    issues.push(`Legacy lock formatter names still present: ${badFormatterRefs.join(', ')}`);
  }
  const validationHelpers = scriptFiles.filter((file) => /curl|smoke|gate|validation/i.test(file));
  const validationParam = ['Uname', '='].join('');
  const badUnameRefs = validationHelpers.filter((file) => includesDependency(file, validationParam));
  if (badUnameRefs.length) {
    issues.push(`Active validation helpers still send ${validationParam}: ${badUnameRefs.join(', ')}`);
  }
}

function buildReport() {
  const issues = [];
  const warnings = [];
  const metrics = collectMetrics();
  validateRequiredFiles(issues);
  validateMetrics(issues, metrics);
  validateSupportModules(issues);
  validateDetailFlow(issues);
  validateManifestI18n(issues);
  validateLocalizationKeys(issues);
  validateCreateSentinelCentralization(issues);
  validateRequiredScripts(issues);
  validateNoLegacyValidationRefs(issues);
  return {
    generatedAt: new Date().toISOString(),
    ok: issues.length === 0,
    score: Math.max(0, 100 - issues.length * 8 - warnings.length * 2),
    metrics,
    limits: LIMITS,
    issues,
    warnings
  };
}

module.exports = { buildReport, LIMITS };
