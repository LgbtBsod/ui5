const path = require('path');
const { readJsonSafe } = require('../../lib/io-runtime');

const ROOT = path.resolve(__dirname, '..', '..', '..');

function loadRules(fileName) {
  return readJsonSafe(path.join(ROOT, 'udos', 'economy', 'models', fileName), {});
}

module.exports = {
  loadRules
};
