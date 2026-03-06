const { sha256 } = require('./hashUtils');

function stripComments(text) {
  return text
    .replace(/\/\*[\s\S]*?\*\//g, '')
    .replace(/(^|\s)\/\/.*$/gm, '$1');
}

function normalizeBlock(text) {
  return stripComments(text).replace(/\s+/g, ' ').trim();
}

function fingerprint(text) {
  return sha256(normalizeBlock(text));
}

module.exports = { normalizeBlock, fingerprint };
