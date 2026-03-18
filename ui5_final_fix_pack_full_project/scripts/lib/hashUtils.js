const crypto = require('crypto');

function sha256(text) {
  return crypto.createHash('sha256').update(text).digest('hex');
}

function rollingHashBlocks(text, blockSize = 64) {
  const values = [];
  for (let i = 0; i < text.length; i += blockSize) {
    values.push(sha256(text.slice(i, i + blockSize)).slice(0, 16));
  }
  return values;
}

module.exports = { sha256, rollingHashBlocks };
