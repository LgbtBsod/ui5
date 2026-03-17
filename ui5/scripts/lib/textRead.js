const fs = require('fs');
const path = require('path');

function normalizeLineEndings(text) {
  return text.replace(/\r\n?/g, '\n');
}

function readSafe(root, relPath) {
  const absPath = path.join(root, relPath);
  try {
    const raw = fs.readFileSync(absPath, 'utf8');
    const text = normalizeLineEndings(raw);
    return {
      ok: true,
      path: absPath,
      text,
      lines: text.split('\n').length,
      error: null
    };
  } catch (error) {
    return { ok: false, path: absPath, text: '', lines: 0, error };
  }
}

module.exports = { readSafe, normalizeLineEndings };
