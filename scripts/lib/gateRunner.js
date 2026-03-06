const path = require('path');
const { listFiles } = require('./fileWalker');
const { readSafe } = require('./textRead');
const { printViolations, emitJson } = require('./report');

function runGate(config) {
  const root = config.root || path.resolve(__dirname, '../..');
  const include = config.include || (config.roots || []).map((item) => `${item}/**/*.js`);
  const files = listFiles(root, { include, exclude: config.exclude || [] });
  const violations = [];

  files.forEach((file) => {
    const read = readSafe(root, file);
    if (!read.ok) {
      violations.push({ file, line: null, message: `read error: ${read.error.message}` });
      return;
    }
    const result = config.check({ file, text: read.text, lines: read.lines }) || [];
    result.forEach((item) => violations.push(item));
  });

  printViolations(config.name, violations);
  emitJson({ gate: config.name, files: files.length, violations });
  if (violations.length) process.exit(1);
}

module.exports = { runGate };
