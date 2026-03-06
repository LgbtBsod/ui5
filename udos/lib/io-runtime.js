const fs = require('fs');

function readTextSafe(filePath, fallback = '') {
  try {
    return fs.readFileSync(filePath, 'utf8');
  } catch (_error) {
    return fallback;
  }
}

function readJsonSafe(filePath, fallback) {
  try {
    return JSON.parse(readTextSafe(filePath, ''));
  } catch (_error) {
    return fallback;
  }
}

function readJsonLinesSafe(filePath, fallback = []) {
  const text = readTextSafe(filePath, '');
  if (!text) {
    return Array.isArray(fallback) ? fallback : [];
  }
  return text
    .split('\n')
    .filter(Boolean)
    .map(function (line) {
      try {
        return JSON.parse(line);
      } catch (_error) {
        return null;
      }
    })
    .filter(Boolean);
}

module.exports = {
  readTextSafe,
  readJsonSafe,
  readJsonLinesSafe
};
