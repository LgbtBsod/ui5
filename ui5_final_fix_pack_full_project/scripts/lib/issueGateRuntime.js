const { exitWithMappedIssues } = require('./gate-result');

function toColonIssue(line, fallbackFile) {
  const text = String(line || '');
  const index = text.indexOf(':');
  const file = index > 0 ? text.slice(0, index) : String(fallbackFile || '');
  return { file, message: text };
}

function exitWithColonIssues(name, issues, stats, opts, fallbackFile) {
  exitWithMappedIssues(
    name,
    issues,
    function (line) {
      return toColonIssue(line, fallbackFile);
    },
    stats,
    opts
  );
}

module.exports = {
  toColonIssue,
  exitWithColonIssues
};
