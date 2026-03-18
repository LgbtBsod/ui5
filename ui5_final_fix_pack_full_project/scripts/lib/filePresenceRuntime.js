const fs = require('fs');

function missingPaths(requiredPaths) {
  return (requiredPaths || []).filter((filePath) => !fs.existsSync(filePath));
}

module.exports = {
  missingPaths
};
