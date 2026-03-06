#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { listFiles } = require('../lib/fileWalker');
const { readJsonSafe } = require('../lib/auditInput');
const { exitWithGateResult } = require('../lib/gate-result');

const ROOT = path.resolve(__dirname, '../..');
const ALLOWLIST_PATH = path.join(__dirname, 'shadow-duplicate-allowlist.json');

const INCLUDE = ['controller/**/*.js', 'service/**/*.js', 'infra/**/*.js', 'util/**/*.js', 'manager/**/*.js'];
const EXCLUDE = ['scripts/**', 'docs/**', 'mock_gate_way/**', 'sap_backend/**', 'node_modules/**', 'dist/**'];

const CRITICAL_SUFFIX_RE = /(Facade|UseCase|Usecase|Adapter|Manager|Coordinator|Loader|Policy|Builder|Mapper)\.js$/i;
const CRITICAL_STATE_RE = /State(Paths|Schema)[^/]*\.js$/i;

function readAllowlist() {
  if (!fs.existsSync(ALLOWLIST_PATH)) return new Set();
  const parsed = readJsonSafe(ALLOWLIST_PATH, null);
  if (!Array.isArray(parsed)) {
    console.error(`FAIL shadow-duplicate-gate`);
    console.error(`- invalid allowlist JSON: ${ALLOWLIST_PATH}`);
    process.exit(1);
  }
  return new Set(parsed.map((item) => item && item.path).filter(Boolean));
}

function isCritical(file) {
  const base = path.basename(file);
  return CRITICAL_SUFFIX_RE.test(base) || CRITICAL_STATE_RE.test(base);
}

function featureLocation(file) {
  const p = file.split('/');
  if (p[0] === 'service' && p[1] === 'domain' && p[2]) return `service/domain/${p[2]}`;
  if (p[0] === 'service' && p[1]) return `service/${p[1]}`;
  if (p[0] === 'util' && p[1]) return `util/${p[1]}`;
  if (p[0] === 'infra' && p[1]) return `infra/${p[1]}`;
  if (p[0] === 'controller' && p[1]) return `controller/${p[1]}`;
  if (p[0] === 'manager' && p[1]) return `manager/${p[1]}`;
  return p[0] || 'root';
}

function makeViolation({ basename, severity, paths, ruleId }) {
  const file = paths[0];
  return {
    ruleId,
    severity,
    file,
    message: `Duplicate critical module basename detected: ${basename}. This indicates shadow legacy.`,
    evidence: paths,
    fixHint: [
      'Delete the shadow module if unused.',
      'If used, migrate call sites to the canonical location (service/domain or facades) and delete the duplicate.'
    ],
    goodExample: 'Controller imports facades/SearchFacade; only one SearchFacade exists.',
    badExample: 'Both service/search/SearchFacade.js and service/domain/search/SearchFacade.js exist.'
  };
}

function groupByBasename(files) {
  return files.reduce((acc, file) => {
    const base = path.basename(file);
    acc[base] = acc[base] || [];
    acc[base].push(file);
    return acc;
  }, {});
}

function isAllowlisted(paths, allowlist) {
  return paths.some((p) => allowlist.has(p));
}

function hasShadowPattern(paths) {
  const hasServiceDomain = paths.some((p) => p.startsWith('service/domain/'));
  const hasLegacyShadowFolder = paths.some((p) => /^service\/(search|detail|autosave)\//.test(p));
  if (hasServiceDomain && hasLegacyShadowFolder) return true;

  const hasUtilSearchOrDetail = paths.some((p) => /^util\/(search|detail)\//.test(p));
  if (hasServiceDomain && hasUtilSearchOrDetail) return true;

  const hasServiceFramework = paths.some((p) => p.startsWith('service/framework/'));
  const hasFramework = paths.some((p) => p.startsWith('framework/'));
  if (hasServiceFramework && hasFramework) return true;

  return false;
}

(function main() {
  const allowlist = readAllowlist();
  const files = listFiles(ROOT, { include: INCLUDE, exclude: EXCLUDE }).filter(isCritical);
  const groups = groupByBasename(files);

  const violations = [];
  const duplicateNames = [];

  Object.entries(groups).forEach(([basename, paths]) => {
    if (paths.length < 2) return;

    const featureLocations = new Set(paths.map(featureLocation));
    const isRuleA = featureLocations.size >= 2;
    const isRuleB = hasShadowPattern(paths);

    if (!isRuleA && !isRuleB) return;
    if (isAllowlisted(paths, allowlist)) return;

    duplicateNames.push(basename);
    violations.push(makeViolation({
      basename,
      severity: isRuleB ? 'HIGH' : 'MEDIUM',
      paths,
      ruleId: 'shadow-dup.duplicate-critical-module'
    }));
  });

  const stats = {
    scannedFiles: files.length,
    duplicateGroupsCount: violations.length,
    topDuplicateNames: duplicateNames.slice(0, 10)
  };

  exitWithGateResult('shadow-duplicate-gate', violations, stats, {
    asJson: process.argv.includes('--json')
  });
})();
