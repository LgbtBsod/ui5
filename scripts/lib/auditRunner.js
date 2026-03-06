const path = require('path');
const { writeJsonAndMarkdown } = require('./reportFiles');

function runJsonMarkdownAudit(options) {
  const cfg = options || {};
  const root = cfg.root || process.cwd();
  const buildReport = typeof cfg.buildReport === 'function' ? cfg.buildReport : function () { return {}; };
  const toMarkdown = typeof cfg.toMarkdown === 'function' ? cfg.toMarkdown : function () { return ''; };
  const report = buildReport();
  writeJsonAndMarkdown(cfg.outJson, cfg.outMd, report, toMarkdown);

  if (typeof cfg.logLine === 'function') {
    console.log(cfg.logLine(report, root));
  }
  return report;
}

module.exports = {
  runJsonMarkdownAudit
};
