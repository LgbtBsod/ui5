const path = require('path');
const { listFiles } = require('./fileWalker');

function listCssFiles(rootDir, cssRootAbs) {
  const relRoot = path.relative(rootDir, cssRootAbs).replace(/\\/g, '/');
  if (!relRoot || relRoot.startsWith('..')) return [];
  return listFiles(rootDir, { include: [`${relRoot}/**/*.css`] })
    .map((rel) => path.join(rootDir, rel))
    .sort();
}

function toProjectRel(rootDir, absPath) {
  return path.relative(rootDir, absPath).replace(/\\/g, '/');
}

module.exports = {
  listCssFiles,
  toProjectRel
};
