const fs = require("fs");
const path = require("path");

const root = path.resolve(__dirname, "..", "app");
const exts = new Set([".js", ".xml", ".css"]);
const offenders = [];

function walk(dir) {
  fs.readdirSync(dir, { withFileTypes: true }).forEach((entry) => {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      walk(full);
      return;
    }
    if (!exts.has(path.extname(entry.name))) {
      return;
    }
    if (/\r\n/.test(fs.readFileSync(full, "utf8"))) {
      offenders.push(path.relative(path.resolve(__dirname, ".."), full));
    }
  });
}

walk(root);

if (offenders.length) {
  console.error("CRLF detected in:");
  offenders.forEach((file) => console.error(file));
  process.exit(1);
}
