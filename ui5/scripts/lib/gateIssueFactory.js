function createIssue(config) {
  const issue = config || {};
  return {
    ruleId: String(issue.ruleId || ''),
    severity: String(issue.severity || 'MEDIUM'),
    file: String(issue.file || ''),
    message: String(issue.message || ''),
    evidence: issue.evidence,
    fixHint: issue.fixHint,
    goodExample: issue.goodExample,
    badExample: issue.badExample,
    doc: issue.doc,
    suggestedPatch: issue.suggestedPatch
  };
}

module.exports = {
  createIssue
};
