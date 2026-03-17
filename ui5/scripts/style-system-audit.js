#!/usr/bin/env node
const path = require('path');
const { runJsonMarkdownAudit } = require('./lib/auditRunner');
const { listCssFiles, toProjectRel } = require('./lib/cssFiles');
const { countMatches, readTextSafe } = require('./lib/auditInput');
const { buildAuditReport } = require('./lib/auditReportFactory');
const { renderStyleSystemAuditMarkdown } = require('./lib/auditMarkdownProfiles');

const ROOT = process.cwd();
const CSS_DIR = path.join(ROOT, 'app', 'styles');
const OUT_DIR = path.join(ROOT, 'docs', 'artifacts');
const OUT_JSON = path.join(OUT_DIR, 'style-system-audit.json');
const OUT_MD = path.join(OUT_DIR, 'style-system-audit.md');

function rel(file) {
  return toProjectRel(ROOT, file);
}

function analyze() {
  const cssFiles = listCssFiles(ROOT, CSS_DIR);
  const variableMap = new Map();
  const controlSelectors = {
    switch: /\.sapMSwt\b/g,
    button: /\.sapMBtnInner\b/g,
    input: /\.sapMInputBaseContentWrapper\b/g,
    dialog: /\.sapMDialog\b/g,
    table: /\.sapMListTbl(Row|Cell|HeaderCell)?\b/g,
    popover: /\.sapMPopover\b/g,
    scrollbar: /::-webkit-scrollbar/g
  };
  const coverage = Object.fromEntries(Object.keys(controlSelectors).map((k) => [k, 0]));
  let backdropUsage = 0;
  let glassUsage = 0;

  cssFiles.forEach((file) => {
    const text = readTextSafe(file);
    const re = /(--[A-Za-z0-9_-]+)\s*:/g;
    let m;
    while ((m = re.exec(text))) {
      const name = m[1];
      if (!variableMap.has(name)) {
        variableMap.set(name, new Set());
      }
      variableMap.get(name).add(rel(file));
    }
    Object.keys(controlSelectors).forEach((key) => {
      coverage[key] += countMatches(text, controlSelectors[key]);
    });
    backdropUsage += countMatches(text, /backdrop-filter\s*:/g);
    glassUsage += countMatches(text, /glass(Card|Dialog|Table|Filter)|platformPrecisionEnterprise|platformCalmModern/g);
  });

  const duplicates = [...variableMap.entries()]
    .filter(([, files]) => files.size > 1)
    .map(([name, files]) => ({ name, files: [...files].sort() }))
    .sort((a, b) => a.name.localeCompare(b.name));

  const themeMixin = readTextSafe(path.join(ROOT, 'app', 'controller', 'base', 'ThemeMixin.js'));
  const themeService = readTextSafe(path.join(ROOT, 'app', 'service', 'framework', 'ThemeService.js'));
  const themePhilosophy = readTextSafe(path.join(ROOT, 'app', 'service', 'framework', 'ThemePhilosophy.js'));
  const indexHtml = readTextSafe(path.join(ROOT, 'app', 'index.html'));
  const styleCss = cssFiles.map((file) => readTextSafe(file)).join('\n');

  const contracts = {
    fiori3MorningDefault: /data-sap-ui-theme="sap_fiori_3"/.test(indexHtml) || /sap_fiori_3/.test(themeService),
    safeNightFallback: !/sap_horizon/.test(indexHtml) && /return DEFAULT_MODE;/.test(themeService) && /sNextMode = "morning"/.test(themeMixin),
    morningNightModes: /:root\.light-mode/.test(styleCss) && /body\.appDark/.test(styleCss),
    platformPhilosophyBridge: /platformPrecisionEnterprise/.test(styleCss) &&
      /platformCalmModern/.test(styleCss) &&
      /platformPrecisionEnterprise/.test(themePhilosophy) &&
      /platformCalmModern/.test(themePhilosophy),
    glassSurfaceLanguage: backdropUsage > 0 && glassUsage > 0
  };

  const score = Math.max(
    0,
    100
      - Math.min(18, duplicates.length)
      - (contracts.fiori3MorningDefault ? 0 : 20)
      - (contracts.safeNightFallback ? 0 : 20)
      - (contracts.morningNightModes ? 0 : 15)
      - (contracts.platformPhilosophyBridge ? 0 : 12)
      - (contracts.glassSurfaceLanguage ? 0 : 10)
  );

  return buildAuditReport({
    cssFileCount: cssFiles.length,
    customPropertyCount: variableMap.size,
    duplicateCustomPropertyCount: duplicates.length,
    duplicateCustomProperties: duplicates.slice(0, 40),
    controlCoverage: coverage,
    backdropUsage,
    glassUsage,
    contracts,
    score
  });
}

function toMarkdown(report) {
  return renderStyleSystemAuditMarkdown(report);
}

runJsonMarkdownAudit({
  root: ROOT,
  outJson: OUT_JSON,
  outMd: OUT_MD,
  buildReport: analyze,
  toMarkdown: toMarkdown,
  logLine: function (report) {
    return `Style system audit generated: ${rel(OUT_JSON)} and ${rel(OUT_MD)} (score=${report.score})`;
  }
});
