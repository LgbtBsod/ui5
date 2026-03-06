#!/usr/bin/env node

const path = require('path');
const { buildReport } = require('../lib/finalArchitectureFreezeCore');
const { writeJsonAndMarkdown } = require('../lib/reportFiles');

const ROOT = process.cwd();
const OUT_JSON = path.join(ROOT, 'docs', 'artifacts', 'final-architecture-freeze.json');
const OUT_MD = path.join(ROOT, 'docs', 'artifacts', 'final-architecture-freeze.md');

function toMarkdown(report) {
  return [
    '# Final Architecture Freeze Report',
    '',
    `- Generated at: ${report.generatedAt}`,
    `- OK: ${report.ok}`,
    `- Score: ${report.score}/100`,
    '',
    '## Metrics',
    `- Search.controller.js lines: ${report.metrics.searchControllerLines}/${report.limits.searchControllerLines}`,
    `- Detail.controller.js lines: ${report.metrics.detailControllerLines}/${report.limits.detailControllerLines}`,
    `- Component.js lines: ${report.metrics.componentLines}/${report.limits.componentLines}`,
    `- css/style.css lines: ${report.metrics.styleLines}/${report.limits.styleLines}`,
    '',
    '## Issues',
    ...(report.issues.length ? report.issues.map((item) => `- ${item}`) : ['- none']),
    '',
    '## Warnings',
    ...(report.warnings.length ? report.warnings.map((item) => `- ${item}`) : ['- none'])
  ].join('\n') + '\n';
}

const report = buildReport();
writeJsonAndMarkdown(OUT_JSON, OUT_MD, report, toMarkdown);
if (!report.ok) {
  console.error('FAIL final-architecture-freeze-gate');
  report.issues.forEach((issue) => console.error(` - ${issue}`));
  process.exit(1);
}
console.log('PASS final-architecture-freeze-gate');
