const fs = require('fs');

function createFileExistenceSmoke(prefix, files, detail) {
  const namePrefix = String(prefix || 'smoke');
  const checks = Array.isArray(files) ? files : [];
  const checkDetail = String(detail || 'required file exists');
  return checks.map((file) => ({
    name: `${namePrefix}:${file}`,
    ok: fs.existsSync(file),
    detail: checkDetail
  }));
}

module.exports = {
  createFileExistenceSmoke
};
