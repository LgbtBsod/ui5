#!/usr/bin/env node
const cp = require('child_process');
const fs = require('fs');
const path = require('path');
const { runJsonMarkdownAudit } = require('./lib/auditRunner');
const { countLinesFromRoot, readJsonSafe, readTextSafe } = require('./lib/auditInput');
const { buildAuditReport } = require('./lib/auditReportFactory');
const { renderArchitectAuditMarkdown } = require('./lib/auditMarkdownProfiles');

const ROOT = process.cwd();
const OUT_DIR = path.join(ROOT, 'docs', 'artifacts');
const OUT_JSON = path.join(OUT_DIR, 'architect-audit-report.json');
const OUT_MD = path.join(OUT_DIR, 'architect-audit-report.md');
const SEARCH_DIR = path.join(ROOT, 'app', 'controller', 'search');
const DETAIL_DIR = path.join(ROOT, 'app', 'controller', 'detail');
const FRAMEWORK_DIR = path.join(ROOT, 'app', 'service', 'framework');

function countJsFiles(dirPath) {
  if (!fs.existsSync(dirPath)) {
    return 0;
  }
  return fs.readdirSync(dirPath, { withFileTypes: true })
    .filter((entry) => entry.isFile() && entry.name.endsWith('.js'))
    .length;
}

function parseArchitectureHealth() {
  const text = readTextSafe(path.join(ROOT, 'docs', 'architecture-health.md'));
  const m = text.match(/architecture_health_score:\s*\*\*(\d+(?:\.\d+)?)\/100\*\*|- score:\s*(\d+(?:\.\d+)?)/i);
  return m ? Number(m[1] || m[2]) : null;
}

function computeReadinessScore(qaOk, architectureHealth, styleScore, freezeScore) {
  return Math.round(
    (qaOk ? 100 : 40) * 0.28 +
    (architectureHealth != null ? architectureHealth : 75) * 0.28 +
    (styleScore != null ? styleScore : 72) * 0.22 +
    (freezeScore != null ? freezeScore : 75) * 0.22
  );
}

function readQaStatus() {
  const qa = readJsonSafe(path.join(ROOT, 'docs', 'qa-report-latest.json'), null);
  if (qa && typeof qa.ok === 'boolean') {
    return qa;
  }
  const result = cp.spawnSync(process.execPath, ['scripts/final-static-qa.js'], {
    cwd: ROOT,
    encoding: 'utf8'
  });
  return {
    ok: result.status === 0,
    stats: {
      source: 'live-final-static-qa'
    }
  };
}

function buildReport() {
  const qa = readQaStatus();
  const style = readJsonSafe(path.join(ROOT, 'docs', 'artifacts', 'style-system-audit.json'), null);
  const freeze = readJsonSafe(path.join(ROOT, 'docs', 'artifacts', 'final-architecture-freeze.json'), null);
  const udos = readJsonSafe(path.join(ROOT, 'udos', 'reports', 'analysis-report.json'), null);
  const architectureHealth = parseArchitectureHealth();
  const searchRuntimeModules = countJsFiles(SEARCH_DIR);
  const detailRuntimeModules = countJsFiles(DETAIL_DIR);
  const frameworkRuntimeModules = countJsFiles(FRAMEWORK_DIR);
  const metrics = {
    searchControllerLines: countLinesFromRoot(ROOT, 'app/controller/Search.controller.js')
      + countLinesFromRoot(ROOT, 'app/controller/search/SearchControllerBehavior.js')
      + countLinesFromRoot(ROOT, 'app/controller/search/SearchActionBehavior.js')
      + countLinesFromRoot(ROOT, 'app/controller/search/SearchLifecycleBehavior.js'),
    detailControllerLines: countLinesFromRoot(ROOT, 'app/controller/Detail.controller.js')
      + countLinesFromRoot(ROOT, 'app/controller/detail/DetailControllerBehavior.js')
      + countLinesFromRoot(ROOT, 'app/controller/detail/DetailControllerRuntime.js')
      + countLinesFromRoot(ROOT, 'app/controller/detail/DetailChecklistBehavior.js'),
    componentLines: countLinesFromRoot(ROOT, 'app/Component.js'),
    styleLines: countLinesFromRoot(ROOT, 'app/styles/app-styles.css'),
    searchRuntimeModules,
    detailRuntimeModules,
    frameworkRuntimeModules
  };
  const zonesOfGrowth = [];

  if (metrics.searchControllerLines > 450) zonesOfGrowth.push('Search controller/runtime surface remains large and should be split by bounded user-flow ownership.');
  if (metrics.detailControllerLines > 800) zonesOfGrowth.push('Detail controller/runtime surface remains oversized and should keep shedding orchestration.');
  if (searchRuntimeModules > 16) zonesOfGrowth.push(`Search runtime is fragmented across ${searchRuntimeModules} modules; consolidate lifecycle/navigation/selection ownership.`);
  if (detailRuntimeModules > 24) zonesOfGrowth.push(`Detail runtime is fragmented across ${detailRuntimeModules} modules; collapse route/edit/attachment/validation paths.`);
  if (frameworkRuntimeModules > 90) zonesOfGrowth.push(`Framework runtime layer is oversized at ${frameworkRuntimeModules} modules; remove indirection with no boundary value.`);
  if (metrics.componentLines > 700) zonesOfGrowth.push('Component bootstrap remains heavy and is a candidate for further startup extraction.');
  if (style && style.duplicateCustomPropertyCount > 24) zonesOfGrowth.push('CSS token duplication is still high; continue collapsing repeated custom properties.');
  if (udos && udos.memory && udos.memory.recordedEvents < 5) zonesOfGrowth.push('UDOS memory corpus is still shallow; more historical events improve adaptive governance quality.');
  if (freeze && !freeze.ok) zonesOfGrowth.push('Final architecture freeze gate is not green; structural contract drift must be resolved first.');
  if (architectureHealth == null) zonesOfGrowth.push('Architecture health score artifact is missing; either generate it consistently or remove it from readiness math.');
  if (!style) zonesOfGrowth.push('Style-system audit artifact is missing; architect audit still depends on fallback scoring.');
  if (!freeze) zonesOfGrowth.push('Final architecture freeze artifact is missing; readiness is still partially heuristic.');
  if (!udos) zonesOfGrowth.push('UDOS analysis artifact is missing; either generate it or drop the dependency from architect audit.');

  const strengths = [
    qa.ok ? 'QA pipeline passes green across the active architecture gates.' : 'QA pipeline is not green.',
    architectureHealth != null ? `Architecture health score is ${architectureHealth}/100.` : 'Architect audit falls back gracefully when architecture-health evidence is absent.',
    style ? `Style-system audit score is ${style.score}/100 with Horizon/Cupertino contracts tracked.` : 'Style-system audit is optional and no longer presented as a project strength when absent.',
    freeze ? `Final architecture freeze score is ${freeze.score}/100.` : 'Final architecture freeze evidence is currently optional and treated as audit debt, not as a hidden blocker.',
    udos ? `UDOS decision is ${udos.decision} with gitless-compatible governance flow.` : 'UDOS evidence is optional and does not block runtime/governance checks.'
  ];

  const readinessScore = computeReadinessScore(
    !!qa.ok,
    architectureHealth,
    style ? style.score : null,
    freeze ? freeze.score : null
  );

  return buildAuditReport({
    readinessScore,
    qaOk: !!qa.ok,
    architectureHealth,
    styleScore: style ? style.score : null,
    freezeScore: freeze ? freeze.score : null,
    freezeOk: freeze ? !!freeze.ok : null,
    udosDecision: udos ? udos.decision : null,
    metrics,
    strengths,
    zonesOfGrowth
  });
}

function toMarkdown(report) {
  return renderArchitectAuditMarkdown(report);
}

runJsonMarkdownAudit({
  root: ROOT,
  outJson: OUT_JSON,
  outMd: OUT_MD,
  buildReport: buildReport,
  toMarkdown: toMarkdown,
  logLine: function (report, root) {
    return `Architect audit generated: ${path.relative(root, OUT_JSON)} and ${path.relative(root, OUT_MD)} (readiness=${report.readinessScore})`;
  }
});
