const fs = require('fs');
const { readJsonSafe } = require('./auditInput');

function failWith(prefix, message, exitCode) {
  const code = Number.isInteger(exitCode) ? exitCode : 1;
  console.error(`${prefix}: ${message}`);
  process.exit(code);
}

function requireJsonReport(filePath, options) {
  const cfg = options || {};
  const prefix = String(cfg.prefix || 'Report gate failed');
  const missingCode = Number.isInteger(cfg.missingExitCode) ? cfg.missingExitCode : 2;
  const invalidCode = Number.isInteger(cfg.invalidExitCode) ? cfg.invalidExitCode : missingCode;

  if (!fs.existsSync(filePath)) {
    failWith(prefix, `report not found: ${filePath}`, missingCode);
  }

  const report = readJsonSafe(filePath, null);
  if (!report) {
    failWith(prefix, `report is invalid JSON: ${filePath}`, invalidCode);
  }
  return report;
}

module.exports = {
  failWith,
  requireJsonReport
};
