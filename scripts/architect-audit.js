#!/usr/bin/env node
const cp = require('child_process');
const path = require('path');
const { runJsonMarkdownAudit } = require('./lib/auditRunner');
const { countLinesFromRoot, readJsonSafe, readTextSafe } = require('./lib/auditInput');
const { buildAuditReport } = require('./lib/auditReportFactory');
const { renderArchitectAuditMarkdown } = require('./lib/auditMarkdownProfiles');

const ROOT = process.cwd();
const OUT_DIR = path.join(ROOT, 'docs', 'artifacts');
const OUT_JSON = path.join(OUT_DIR, 'architect-audit-report.json');
const OUT_MD = path.join(OUT_DIR, 'architect-audit-report.md');

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
  const metrics = {
    searchControllerLines: countLinesFromRoot(ROOT, 'app/controller/Search.controller.js'),
    detailControllerLines: countLinesFromRoot(ROOT, 'app/controller/Detail.controller.js'),
    componentLines: countLinesFromRoot(ROOT, 'app/Component.js'),
    styleLines: countLinesFromRoot(ROOT, 'app/styles/app-styles.css')
  };
  const zonesOfGrowth = [];

  if (metrics.searchControllerLines > 450) zonesOfGrowth.push('Search controller remains large and should be split by interaction zone.');
  if (metrics.detailControllerLines > 800) zonesOfGrowth.push('Detail controller remains oversized and should keep shedding runtime orchestration.');
  if (metrics.componentLines > 700) zonesOfGrowth.push('Component bootstrap remains heavy and is a candidate for further startup extraction.');
  if (style && style.duplicateCustomPropertyCount > 24) zonesOfGrowth.push('CSS token duplication is still high; continue collapsing repeated custom properties.');
  if (udos && udos.memory && udos.memory.recordedEvents < 5) zonesOfGrowth.push('UDOS memory corpus is still shallow; more historical events improve adaptive governance quality.');
  if (freeze && !freeze.ok) zonesOfGrowth.push('Final architecture freeze gate is not green; structural contract drift must be resolved first.');

  const strengths = [
    qa.ok ? 'QA pipeline passes green across the active architecture gates.' : 'QA pipeline is not green.',
    architectureHealth != null ? `Architecture health score is ${architectureHealth}/100.` : 'Architecture health score artifact is missing.',
    style ? `Style-system audit score is ${style.score}/100 with Horizon/Cupertino contracts tracked.` : 'Style-system audit artifact missing.',
    freeze ? `Final architecture freeze score is ${freeze.score}/100.` : 'Final architecture freeze artifact missing.',
    udos ? `UDOS decision is ${udos.decision} with gitless-compatible governance flow.` : 'UDOS analysis artifact missing.'
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
