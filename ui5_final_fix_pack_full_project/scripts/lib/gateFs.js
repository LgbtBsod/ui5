const fs = require('fs');
const { readJsonSafe, readTextSafe } = require('./auditInput');

function missingPaths(paths) {
  return paths.filter((item) => !fs.existsSync(item));
}

function fail(message) {
  console.error(message);
  process.exit(1);
}

module.exports = {
  missingPaths,
  readText: readTextSafe,
  readJson: readJsonSafe,
  fail
};
