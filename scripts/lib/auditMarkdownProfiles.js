const { buildAuditMarkdown, formatKeyValueLines, formatStringLines } = require('./auditMarkdownBuilder');

function renderArchitectAuditMarkdown(report) {
  return buildAuditMarkdown({
    title: 'Architect Audit Report',
    summaryPairs: [
      ['Generated at', report.generatedAt],
      ['Readiness score', `${report.readinessScore}/100`],
      ['QA green', report.qaOk],
      ['Architecture health', report.architectureHealth ?? '-'],
      ['Style-system score', report.styleScore ?? '-'],
      ['Final architecture freeze', `${report.freezeScore ?? '-'} (${report.freezeOk})`],
      ['UDOS decision', report.udosDecision || '-']
    ],
    sections: [
      {
        title: 'Metrics',
        lines: formatKeyValueLines([
          ['Search.controller.js lines', report.metrics.searchControllerLines],
          ['Detail.controller.js lines', report.metrics.detailControllerLines],
          ['Component.js lines', report.metrics.componentLines],
          ['css/style.css lines', report.metrics.styleLines]
        ])
      },
      {
        title: 'Strengths',
        lines: formatStringLines(report.strengths)
      },
      {
        title: 'Zones Of Growth',
        lines: formatStringLines(
          report.zonesOfGrowth,
          '- none identified by the current automated heuristics'
        )
      }
    ]
  });
}

function renderStyleSystemAuditMarkdown(report) {
  return buildAuditMarkdown({
    title: 'Style System Audit',
    summaryPairs: [
      ['Generated at', report.generatedAt],
      ['Score', `${report.score}/100`],
      ['CSS files', report.cssFileCount],
      ['Custom properties', report.customPropertyCount],
      ['Duplicate custom properties', report.duplicateCustomPropertyCount]
    ],
    sections: [
      {
        title: 'Theme Contracts',
        lines: formatKeyValueLines([
          ['Horizon Morning default', report.contracts.horizonMorningDefault],
          ['Horizon Night supported', report.contracts.horizonNightSupported],
          ['Morning/Night CSS modes', report.contracts.morningNightModes],
          ['Platform philosophy bridge present', report.contracts.platformPhilosophyBridge],
          ['Glass surface language present', report.contracts.glassSurfaceLanguage]
        ])
      },
      {
        title: 'Control Coverage',
        lines: formatKeyValueLines(
          Object.keys(report.controlCoverage).map(function (key) {
            return [key, report.controlCoverage[key]];
          })
        )
      },
      {
        title: 'Duplicate Custom Properties',
        lines: formatKeyValueLines(
          report.duplicateCustomProperties.map(function (entry) {
            return [entry.name, entry.files.join(', ')];
          }),
          '- none'
        )
      }
    ]
  });
}

module.exports = {
  renderArchitectAuditMarkdown,
  renderStyleSystemAuditMarkdown
};
