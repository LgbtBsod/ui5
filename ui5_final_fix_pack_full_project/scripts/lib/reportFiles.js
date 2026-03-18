const fs = require('fs');
const path = require('path');

function ensureDirFor(file) {
  fs.mkdirSync(path.dirname(file), { recursive: true });
}

function writeTextFile(file, content) {
  ensureDirFor(file);
  fs.writeFileSync(file, content, 'utf8');
}

function writeJsonFile(file, payload) {
  writeTextFile(file, JSON.stringify(payload, null, 2) + '\n');
}

function writeJsonAndMarkdown(jsonFile, mdFile, payload, toMarkdown) {
  writeJsonFile(jsonFile, payload);
  writeTextFile(mdFile, toMarkdown(payload));
}

module.exports = {
  ensureDirFor,
  writeJsonAndMarkdown,
  writeJsonFile,
  writeTextFile
};
