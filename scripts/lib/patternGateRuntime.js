const { runGate } = require('./gateRunner');
const { scanPatterns } = require('./patternScan');

const DEFAULT_INCLUDE = ['controller/**/*.js', 'service/**/*.js', 'infra/**/*.js', 'manager/**/*.js', 'util/**/*.js', 'model/**/*.js', 'ports/**/*.js'];

function runPatternGate(name, patterns, opts) {
  const options = opts || {};
  const include = options.include || DEFAULT_INCLUDE;
  const shouldSkip = typeof options.shouldSkip === 'function' ? options.shouldSkip : function () { return false; };

  runGate({
    name,
    include,
    check: function ({ file, text }) {
      if (shouldSkip(file, text)) return [];
      return scanPatterns(file, text, patterns || []);
    }
  });
}

module.exports = {
  runPatternGate
};
