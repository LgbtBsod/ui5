const { renderAuditMarkdown, renderListEntries, renderSummaryEntries } = require('./markdownAudit');

function formatKeyValueLines(entries, fallbackLine) {
  return renderListEntries(
    entries,
    function (entry) {
      return `- ${entry[0]}: ${entry[1]}`;
    },
    fallbackLine
  );
}

function formatStringLines(entries, fallbackLine) {
  return renderListEntries(
    entries,
    function (entry) {
      return `- ${entry}`;
    },
    fallbackLine
  );
}

function buildAuditMarkdown(config) {
  const summaryPairs = Array.isArray(config && config.summaryPairs) ? config.summaryPairs : [];
  return renderAuditMarkdown({
    title: config && config.title,
    summary: renderSummaryEntries(summaryPairs.map(function (pair) {
      return { label: pair[0], value: pair[1] };
    })),
    sections: Array.isArray(config && config.sections) ? config.sections : []
  });
}

module.exports = {
  buildAuditMarkdown,
  formatKeyValueLines,
  formatStringLines
};
