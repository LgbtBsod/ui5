const { missingPaths, readJson, readText, fail } = require('./gateFs');

function runJsonContractGate(config) {
  const gate = config || {};
  const dataPath = String(gate.dataPath || '');
  if (missingPaths([dataPath]).length) {
    fail(String(gate.missingMessage || 'Artifact JSON is missing.'));
  }
  const data = readJson(dataPath);
  const validator = typeof gate.validate === 'function' ? gate.validate : function () { return []; };
  const issues = validator(data) || [];
  if (Array.isArray(issues) && issues.length) {
    fail(String(issues[0]));
  }
  console.log(String(gate.passMessage || 'Gate passed.'));
}

function runTextContractGate(config) {
  const gate = config || {};
  const dataPath = String(gate.dataPath || '');
  if (missingPaths([dataPath]).length) {
    fail(String(gate.missingMessage || 'Artifact text is missing.'));
  }
  const text = readText(dataPath);
  const validator = typeof gate.validate === 'function' ? gate.validate : function () { return []; };
  const issues = validator(text) || [];
  if (Array.isArray(issues) && issues.length) {
    fail(String(issues[0]));
  }
  console.log(String(gate.passMessage || 'Gate passed.'));
}

module.exports = {
  runJsonContractGate,
  runTextContractGate
};
