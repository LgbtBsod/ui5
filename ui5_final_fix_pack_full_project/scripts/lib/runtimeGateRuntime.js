const { collectFilesByExtensions } = require('../qa-shared');

function listRuntimeJsFiles(root, entries) {
  const out = [];
  (entries || []).forEach((entry) => {
    if (String(entry || '').endsWith('.js')) {
      out.push(entry);
      return;
    }
    collectFilesByExtensions(root, [entry], ['.js']).forEach((file) => out.push(file));
  });
  return [...new Set(out)].filter((file) => !String(file).startsWith('scripts/')).sort();
}

function pushPipeViolation(violations, file, line, message) {
  violations.push(`- ${file}${line ? `:${line}` : ''} | ${message}`);
}

function mapPipeIssue(line) {
  return { file: String(line).replace(/^- /, '').split('|')[0].trim(), message: String(line) };
}

module.exports = {
  listRuntimeJsFiles,
  pushPipeViolation,
  mapPipeIssue
};
