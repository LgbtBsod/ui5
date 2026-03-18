const { listFiles } = require('../../lib/fileWalker');
const { readSafe } = require('../../lib/textRead');

function scanMatches(root, include, predicate) {
  return listFiles(root, { include })
    .map((file) => ({ file, read: readSafe(root, file) }))
    .filter(({ file, read }) => read.ok && predicate(file, read.text));
}

function buildFilePatch(file, beforeLines, afterLines) {
  return [
    `--- a/${file}`,
    `+++ b/${file}`,
    '@@',
    ...(beforeLines || []).map((line) => `-${line}`),
    ...(afterLines || []).map((line) => `+${line}`)
  ].join('\n');
}

function proposeTodoFix(matches, todoLine) {
  return (matches || []).map(function (item) {
    return {
      file: item.file,
      patch: buildFilePatch(item.file, [], [String(todoLine || '')]),
      safeApply: false
    };
  });
}

function createRule(config) {
  const rule = config || {};
  return {
    id: String(rule.id || ''),
    description: String(rule.description || ''),
    detect: typeof rule.detect === 'function' ? rule.detect : function () { return []; },
    proposeFix: typeof rule.proposeFix === 'function' ? rule.proposeFix : function () { return []; }
  };
}

function createTodoRule(config) {
  const rule = config || {};
  return createRule({
    id: rule.id,
    description: rule.description,
    detect: function (root) {
      return scanMatches(root, rule.include || ['**/*.js'], function (file, text) {
        if (typeof rule.predicate === 'function') {
          return rule.predicate(file, text);
        }
        return false;
      });
    },
    proposeFix: function (matches) {
      return proposeTodoFix(matches, rule.todo || '// TODO(autofix)');
    }
  });
}

module.exports = {
  createRule,
  createTodoRule,
  scanMatches,
  buildFilePatch,
  proposeTodoFix
};
