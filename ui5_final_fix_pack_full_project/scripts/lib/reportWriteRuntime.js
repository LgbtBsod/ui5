const fs = require('fs');
const path = require('path');

function ensureDirFor(filePath) {
  fs.mkdirSync(path.dirname(filePath), { recursive: true });
}

function writeJsonAndMarkdown(jsonPath, jsonValue, markdownPath, markdownLines) {
  ensureDirFor(jsonPath);
  ensureDirFor(markdownPath);
  fs.writeFileSync(jsonPath, JSON.stringify(jsonValue, null, 2));
  fs.writeFileSync(markdownPath, (markdownLines || []).join('\n'));
}

module.exports = {
  writeJsonAndMarkdown
};
