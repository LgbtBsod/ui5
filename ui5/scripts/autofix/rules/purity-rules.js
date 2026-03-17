const { createTodoRule } = require('./shared');

const managerPurityRule = createTodoRule({
  id: 'manager-purity-rule',
  description: 'Suggest moving manager model writes into usecase effects',
  include: ['service/runtime/**/*.js'],
  predicate: function (_file, text) {
    return /setProperty\(|setData\(/.test(text);
  },
  todo: '// TODO(autofix): move manager model mutation into tick usecase; return modelPatch effect'
});

const stylePurityRule = createTodoRule({
  id: 'style-purity-rule',
  description: 'Insert TODO for style mutations outside Ui5StyleAdapter',
  include: ['**/*.js'],
  predicate: function (file, text) {
    return file !== 'infra/adapters/Ui5StyleAdapter.js' && /addStyleClass\(|removeStyleClass\(/.test(text);
  },
  todo: '// TODO(autofix): route style change via styleToken effects in Ui5StyleAdapter'
});

module.exports = {
  managerPurityRule,
  stylePurityRule
};
