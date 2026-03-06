function nowIso() {
  return new Date().toISOString();
}

function buildAuditReport(payload) {
  return Object.assign({
    generatedAt: nowIso()
  }, payload || {});
}

module.exports = {
  buildAuditReport,
  nowIso
};
