const fs = require('fs');
const path = require('path');

function readTextSafe(file, fallback = '') {
  try {
    return fs.readFileSync(file, 'utf8');
  } catch (error) {
    return fallback;
  }
}

function readJsonSafe(file, fallback) {
  try {
    return JSON.parse(fs.readFileSync(file, 'utf8'));
  } catch (error) {
    return fallback;
  }
}

function readJsonLinesSafe(file, fallback = []) {
  const text = readTextSafe(file, '');
  if (!text) {
    return Array.isArray(fallback) ? fallback : [];
  }
  return text
    .split(/\r?\n/)
    .filter(Boolean)
    .map((line) => {
      try {
        return JSON.parse(line);
      } catch (_error) {
        return null;
      }
    })
    .filter(Boolean);
}

function countLinesFromRoot(root, relFile) {
  const text = readTextSafe(path.join(root, relFile), '');
  return text ? text.split(/\r?\n/).length : 0;
}

function countMatches(text, regex) {
  const matches = String(text).match(regex);
  return matches ? matches.length : 0;
}

module.exports = {
  countLinesFromRoot,
  countMatches,
  readJsonSafe,
  readJsonLinesSafe,
  readTextSafe
};
