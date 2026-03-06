const { buildFilePatch, createRule, scanMatches } = require('./shared');

module.exports = createRule({
  id: 'feedback-import-rule',
  description: 'Remove unused MessageBox/MessageToast imports outside EffectApplier',
  detect: function (root) {
    return scanMatches(
      root,
      ['controller/**/*.js', 'service/**/*.js', 'infra/**/*.js', 'manager/**/*.js', 'util/**/*.js'],
      function (file, text) {
        return file !== 'service/framework/EffectApplier.js' && /MessageBox|MessageToast/.test(text);
      }
    );
  },
  proposeFix: function (matches) {
    return matches.map(function (entry) {
      const lines = entry.read.text.split('\n');
      const edits = lines.map(function (line) {
        return /sap\/m\/Message(Box|Toast)/.test(line) ? '' : line;
      });
      return { file: entry.file, patch: buildFilePatch(entry.file, lines, edits), safeApply: false };
    });
  }
});
