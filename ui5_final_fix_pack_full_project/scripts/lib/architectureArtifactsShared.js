const fs = require('fs');
const path = require('path');
const { listFiles } = require('./fileWalker');
const { parseImports } = require('./js-import-parser');
const { writeTextFile, writeJsonFile } = require('./reportFiles');

const ROOT = process.cwd();
const DOCS = path.join(ROOT, 'docs');

function readAbsolute(file) {
  return fs.readFileSync(file, 'utf8');
}

function listRuntimeJsFiles() {
  return listFiles(ROOT, { include: ['**/*.js'] });
}

function writeDoc(relPath, content) {
  writeTextFile(path.join(DOCS, relPath), content);
}

function writeDocJson(relPath, payload) {
  writeJsonFile(path.join(DOCS, relPath), payload);
}

module.exports = {
  DOCS,
  ROOT,
  listRuntimeJsFiles,
  parseImports,
  readAbsolute,
  writeDoc,
  writeDocJson
};
