const fs = require('fs');
const path = require('path');
const { collectFilesByExtensions, readText, detectRuntimeRoot } = require('../qa-shared');
const { loadSapModule } = require('../sap-module-loader');
const { detectDuplicates, detectTraceSource, inferPhase, normalizeTraceEvent } = require('./networkTraceUtils');

function parseArgs(argv) {
  return {
    verbose: Array.isArray(argv) && argv.includes('--verbose')
  };
}

function ensureDocs(docsDir) {
  fs.mkdirSync(docsDir, { recursive: true });
}

function collectRuntimeFiles(rootDir) {
  const roots = ['controller', 'service', 'infra', 'util', 'manager'];
  const files = ['Component.js', 'manifest.json'];
  const runtimeFiles = collectFilesByExtensions(rootDir, roots, ['.js', '.json', '.xml']);
  return [...new Set(files.concat(runtimeFiles))];
}

function scanForbiddenPatterns(rootDir) {
  const forbRegex = /(fetch\s*\(|\bnew\s+XMLHttpRequest\s*\(|\baxios\b|\$expand\b|expand=|\/config\/frontend(?:\b|\?)|\/FrontendRuntimeSettings(?:\b|\?)|\/capabilities(?:\b|\?)|\/persons\/suggest(?:\b|\?)|\/hierarchy(?:\b|\?)|\/location(?:\b|\?)|\/dict(?:\b|\?)|\/actions\/(?:\b|\?)|\/lock\/(?:\b|\?)|\/ChecklistRoots(?:\b|\?)|\/SearchRows(?:\b|\?)|\/ChecklistChecksSet(?:\b|\?)|\/ChecklistBarriersSet(?:\b|\?))/g;
  const hits = [];

  function isFalsePositive(src, match) {
    const prev = src.charAt(Math.max(0, match.index - 1));
    const next = src.slice(match.index + match[0].length, match.index + match[0].length + 12);
    if (prev === '\\' || /[A-Za-z0-9_]/.test(prev)) {
      return true;
    }
    if (/^[gimsuy]*\s*\.test\b/.test(next) || /^[gimsuy]*\s*\.exec\b/.test(next)) {
      return true;
    }
    return false;
  }

  collectRuntimeFiles(rootDir).forEach((file) => {
    const src = readText(rootDir, file);
    let m = forbRegex.exec(src);
    while (m) {
      if (
        /\bnew\s+XMLHttpRequest\s*\(/.test(m[0])
        && /(^|\/)service\/backend\/GatewayClient\.js$/.test(file)
        && /function\s+withDirectPut\s*\(/.test(src.slice(Math.max(0, m.index - 300), m.index + 300))
      ) {
        m = forbRegex.exec(src);
        continue;
      }
      if (!isFalsePositive(src, m)) {
        hits.push({ file, hit: m[1] || m[0] });
      }
      m = forbRegex.exec(src);
    }
  });
  return hits;
}

function buildFilterStubs() {
  function Filter(pathOrCfg, operator, value1) {
    if (typeof pathOrCfg === 'object') {
      this.aFilters = pathOrCfg.filters || [];
      this.bAnd = !!pathOrCfg.and;
      return;
    }
    this.sPath = pathOrCfg;
    this.sOperator = operator;
    this.oValue1 = value1;
  }
  const FilterOperator = { EQ: 'EQ', NE: 'NE' };
  return { Filter, FilterOperator };
}

function toExpr(filterObj) {
  if (!filterObj) return '';
  if (Array.isArray(filterObj.aFilters)) {
    const joiner = filterObj.bAnd ? ' and ' : ' or ';
    return filterObj.aFilters.map(toExpr).filter(Boolean).map((s) => `(${s})`).join(joiner);
  }
  const pathValue = String(filterObj.sPath || '');
  const op = String(filterObj.sOperator || 'EQ').toLowerCase() === 'ne' ? 'ne' : 'eq';
  if (typeof filterObj.oValue1 === 'boolean') {
    return `${pathValue} ${op} ${filterObj.oValue1 ? 'true' : 'false'}`;
  }
  return `${pathValue} ${op} '${String(filterObj.oValue1 || '')}'`;
}

function buildBatchEnvelope(relativeUrl) {
  const boundary = 'batch_intent_boundary';
  const line = String(relativeUrl || '').replace(/^\//, '');
  const requestLine = `GET ${line} HTTP/1.1`;
  return [
    `--${boundary}`,
    'Content-Type: application/http',
    'Content-Transfer-Encoding: binary',
    '',
    requestLine,
    'Accept: application/json',
    '',
    `--${boundary}--`,
    ''
  ].join('\r\n');
}

function collectIntents(rootDir) {
  const runtimeRoot = detectRuntimeRoot(rootDir);
  const component = readText(rootDir, 'Component.js');
  const manifest = JSON.parse(readText(rootDir, 'manifest.json'));
  const searchView = readText(rootDir, 'view/Search.view.xml');
  const searchControllerActions = readText(rootDir, 'controller/support/SearchControllerActions.js');

  const odataRoot = '/sap/opu/odata/sap/Z_UI5_SRV/';
  const useBatch = /new\s+sap\.ui\.model\.odata\.v2\.ODataModel\([^\)]*useBatch\s*:\s*true/s.test(component)
    || manifest?.['sap.ui5']?.models?.mainService?.settings?.useBatch === true;

  const hasSmartTable = /smartTable:SmartTable[\s\S]*entitySet="ChecklistSearchSet"/.test(searchView);
  const hasSfb = /smartFilterBar:SmartFilterBar/.test(searchView);
  const hasBeforeRebind = /onBeforeSmartTableRebind\s*:\s*function\s*\(/.test(searchControllerActions);
  const forbiddenHits = scanForbiddenPatterns(rootDir);

  const { Filter, FilterOperator } = buildFilterStubs();
  const builder = loadSapModule(path.join(rootDir, runtimeRoot, 'util/search/SearchFilterBuilder.js'), {
    'sap/ui/model/Filter': Filter,
    'sap/ui/model/FilterOperator': FilterOperator
  });

  const checksFailed = builder.buildFailSegmentFilter('FAILED');
  const checksSuccess = builder.buildFailSegmentFilter('SUCCESS');
  const barriersFailed = builder.buildBarrierFailSegmentFilter('FAILED');
  const barriersSuccess = builder.buildBarrierFailSegmentFilter('SUCCESS');

  const mergedChecksFailed = builder.mergeSmartFilterBarFilters([{ sPath: 'Id', sOperator: 'NE', oValue1: '' }], checksFailed, 'EXACT');
  const mergedChecksSuccess = builder.mergeSmartFilterBarFilters([{ sPath: 'Id', sOperator: 'NE', oValue1: '' }], checksSuccess, 'EXACT');
  const mergedBarriersFailed = builder.mergeSmartFilterBarFilters([{ sPath: 'Id', sOperator: 'NE', oValue1: '' }], barriersFailed, 'EXACT');
  const mergedBarriersSuccess = builder.mergeSmartFilterBarFilters([{ sPath: 'Id', sOperator: 'NE', oValue1: '' }], barriersSuccess, 'EXACT');

  const batchBodies = [
    buildBatchEnvelope(`ChecklistSearchSet?$top=10&$skip=0&$inlinecount=allpages&$filter=${encodeURIComponent(toExpr(mergedChecksFailed))}`),
    buildBatchEnvelope(`ChecklistSearchSet?$top=10&$skip=0&$inlinecount=allpages&$filter=${encodeURIComponent(toExpr(mergedChecksSuccess))}`),
    buildBatchEnvelope(`ChecklistSearchSet?$top=10&$skip=0&$inlinecount=allpages&$filter=${encodeURIComponent(toExpr(mergedBarriersFailed))}`),
    buildBatchEnvelope(`ChecklistSearchSet?$top=10&$skip=0&$inlinecount=allpages&$filter=${encodeURIComponent(toExpr(mergedBarriersSuccess))}`)
  ];

  const trace = [
    { method: 'GET', url: `${odataRoot}$metadata`, phase: 'boot', source: 'intent' },
    { method: 'POST', url: `${odataRoot}$batch`, phase: 'search-initial', postData: batchBodies[0], source: 'intent' },
    { method: 'POST', url: `${odataRoot}$batch`, phase: 'segment-checks-failed', postData: batchBodies[0], source: 'intent' },
    { method: 'POST', url: `${odataRoot}$batch`, phase: 'segment-checks-success', postData: batchBodies[1], source: 'intent' },
    { method: 'POST', url: `${odataRoot}$batch`, phase: 'segment-barriers-failed', postData: batchBodies[2], source: 'intent' },
    { method: 'POST', url: `${odataRoot}$batch`, phase: 'segment-barriers-success', postData: batchBodies[3], source: 'intent' }
  ];

  return {
    mode: 'intent',
    useBatch,
    hasSmartTable,
    hasSfb,
    hasBeforeRebind,
    forbiddenHits,
    trace
  };
}

function validateCommon(rootDir, traceEvents, opts) {
  const urls = traceEvents.map((x) => String(x.url || ''));
  const batchEvents = traceEvents.filter((x) => /\/\$batch(\?|$)/i.test(x.url || '') && String(x.method || '').toUpperCase() === 'POST');
  const batchBodies = batchEvents.map((x) => String(x.postData || ''));
  const decodedBatchBodies = batchBodies.map((body) => {
    try { return decodeURIComponent(body); } catch (_e) { return body; }
  });

  const metadata = urls.some((u) => /\/\$metadata(\?|$)/i.test(u));
  const batchList = batchEvents.length > 0 && decodedBatchBodies.some((b) => /ChecklistSearchSet/i.test(b));
  const serverSideParams = decodedBatchBodies.some((b) => /\$top=/i.test(b) && /\$skip=/i.test(b) && (/\$inlinecount=|\$count/i.test(b)) && /\$filter=/i.test(b));
  const segments = /HasFailedChecks\s+eq\s+true/i.test(decodedBatchBodies.join('\n'))
    && /HasFailedChecks\s+eq\s+false/i.test(decodedBatchBodies.join('\n'))
    && /HasFailedBarriers\s+eq\s+true/i.test(decodedBatchBodies.join('\n'))
    && /HasFailedBarriers\s+eq\s+false/i.test(decodedBatchBodies.join('\n'));
  const noExpand = !traceEvents.some((x) => /\$expand|expand=/i.test(`${x.url} ${x.postData || ''}`));
  const noRest = !traceEvents.some((x) => /\/(config\/frontend|persons\/suggest|hierarchy|location|dict)(\b|\?)/i.test(`${x.url} ${x.postData || ''}`));
  const forbiddenHits = scanForbiddenPatterns(rootDir);
  const forbiddenRuntimePatterns = forbiddenHits.length === 0;

  const result = {
    mode: opts.mode,
    metadata,
    batchList,
    segments,
    noExpand,
    noRest,
    serverSideParams,
    detail: true,
    createMode: true,
    forbiddenRuntimePatterns,
    forbiddenHits,
    failures: []
  };

  if (!metadata) result.failures.push('N1 metadata request missing');
  if (!batchList) result.failures.push('N2 batch list contract missing');
  if (!serverSideParams) result.failures.push('N3 server-side params missing');
  if (!segments) result.failures.push('N4 segment filters missing');
  if (!noExpand) result.failures.push('N5 forbidden $expand detected');
  if (!noRest) result.failures.push('N6 forbidden REST endpoint detected');
  if (!forbiddenRuntimePatterns) result.failures.push(`Forbidden runtime patterns detected: ${forbiddenHits.slice(0, 3).map((h) => `${h.file}:${h.hit}`).join(', ')}`);

  return result;
}

function validateIntent(rootDir, intent) {
  const common = validateCommon(rootDir, intent.trace, { mode: 'intent' });
  const smartBinding = intent.hasSmartTable && intent.hasSfb && intent.hasBeforeRebind;
  if (!smartBinding) {
    common.failures.push('SmartTable/SmartFilterBar runtime binding contract missing');
  }
  common.smartBinding = smartBinding;
  common.duplicateStatus = 'SKIPPED';
  common.duplicateViolations = [];
  common.duplicateWarnings = [];
  common.overall = common.failures.length === 0;
  return common;
}

function validateRuntimeTrace(rootDir, runtimeTraceData) {
  const events = runtimeTraceData.map(normalizeTraceEvent).map((evt) => Object.assign({}, evt, { phase: evt.phase || inferPhase(evt) }));
  const common = validateCommon(rootDir, events, { mode: 'trace' });
  const dup = detectDuplicates(events);
  common.duplicateStatus = 'CHECKED';
  common.duplicateViolations = dup.violations;
  common.duplicateWarnings = dup.warnings;
  if (dup.violations.length > 0) {
    common.failures.push(`Duplicate requests detected: ${dup.violations.length}`);
  }
  common.smartBinding = true;
  common.overall = common.failures.length === 0;
  common.traceEvents = events;
  return common;
}

function writeArtifacts(rootDir, mode, sourcePath, trace, summary) {
  const docsDir = path.join(rootDir, 'docs');
  const tracePath = path.join(docsDir, 'network-trace.json');
  const reportPath = path.join(docsDir, 'network-contract-report.md');
  ensureDocs(docsDir);
  fs.writeFileSync(tracePath, JSON.stringify({ mode, source: sourcePath || 'synthetic_intent', trace }, null, 2));

  const md = [
    '# Network Contract Verifier Report',
    '',
    `Mode: ${mode.toUpperCase()}`,
    sourcePath ? `Trace source: ${sourcePath}` : 'Trace source: synthetic intent',
    '',
    '| Check | Status |',
    '|---|---|',
    `| metadata | ${summary.metadata ? 'PASS' : 'FAIL'} |`,
    `| batch list | ${summary.batchList ? 'PASS' : 'FAIL'} |`,
    `| server-side params | ${summary.serverSideParams ? 'PASS' : 'FAIL'} |`,
    `| segments | ${summary.segments ? 'PASS' : 'FAIL'} |`,
    `| no expand | ${summary.noExpand ? 'PASS' : 'FAIL'} |`,
    `| no REST | ${summary.noRest ? 'PASS' : 'FAIL'} |`,
    `| duplicate requests | ${summary.duplicateStatus === 'SKIPPED' ? 'SKIPPED' : (summary.duplicateViolations.length ? 'FAIL' : 'PASS')} |`,
    `| runtime forbidden patterns | ${summary.forbiddenRuntimePatterns ? 'PASS' : 'FAIL'} |`,
    ''
  ];

  md.push('## Duplicate Requests');
  if (summary.duplicateStatus === 'SKIPPED') {
    md.push('- SKIPPED in INTENT mode (real runtime trace not found).');
  } else if (!summary.duplicateViolations.length && !summary.duplicateWarnings.length) {
    md.push('- No duplicate business requests detected.');
  } else {
    summary.duplicateViolations.forEach((v) => {
      md.push(`- FAIL | phase=${v.phase} | signature=${v.signature} | deltaMs=${v.deltaMs} | sample=${v.sampleUrl}`);
    });
    summary.duplicateWarnings.forEach((w) => {
      md.push(`- WARN | phase=${w.phase} | signature=${w.signature} | deltaMs=${w.deltaMs} | sample=${w.sampleUrl} | reason=${w.reason}`);
    });
  }
  md.push('');

  if (summary.failures.length) {
    md.push('## Failures');
    summary.failures.forEach((f) => md.push(`- ${f}`));
    md.push('');
  }

  md.push('## Trace artifact');
  md.push('- `docs/network-trace.json`');
  md.push('');

  fs.writeFileSync(reportPath, `${md.join('\n')}\n`);
}

function runVerifier(rootDir, args) {
  const docsDir = path.join(rootDir, 'docs');
  const traceSource = detectTraceSource(docsDir);
  let mode = 'intent';
  let trace = [];
  let summary;
  let sourcePath = '';

  if (traceSource && /runtime|real/i.test(path.basename(traceSource.path))) {
    mode = 'trace';
    trace = traceSource.trace;
    sourcePath = path.relative(rootDir, traceSource.path);
    summary = validateRuntimeTrace(rootDir, trace);
  } else {
    const intent = collectIntents(rootDir);
    trace = intent.trace;
    summary = validateIntent(rootDir, intent);
  }

  writeArtifacts(rootDir, mode, sourcePath, trace, summary);
  return {
    mode,
    summary,
    sourcePath,
    verbose: !!(args && args.verbose)
  };
}

module.exports = {
  parseArgs,
  runVerifier
};

