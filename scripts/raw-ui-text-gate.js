#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const ROOT = process.cwd();
const TARGETS = [
  path.join(ROOT, "app", "controller"),
  path.join(ROOT, "app", "service", "domain")
];
const ALLOWLIST = new Set([]);
const PATTERNS = [
  { regex: /message\s*:\s*"[^"]*[A-Za-z][^"]*\s+[A-Za-z][^"]*"/, label: "raw message text" },
  { regex: /fallback\s*:\s*"[^"]*[A-Za-z][^"]*\s+[A-Za-z][^"]*"/, label: "raw fallback text" }
];
const issues = [];

function walk(dir) {
  if (!fs.existsSync(dir)) {
    return;
  }
  fs.readdirSync(dir, { withFileTypes: true }).forEach((entry) => {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      walk(full);
      return;
    }
    if (!entry.name.endsWith(".js")) {
      return;
    }
    const relative = path.relative(ROOT, full).replace(/\\/g, "/");
    const lines = fs.readFileSync(full, "utf8").split(/\r?\n/);
    lines.forEach((line, index) => {
      const location = `${relative}:${index + 1}`;
      if (ALLOWLIST.has(location)) {
        return;
      }
      PATTERNS.forEach((pattern) => {
        if (pattern.regex.test(line) && !/messageKey|getText|i18n|^[^"]*"[^"]*[A-Z0-9_]{3,}[^"]*"/.test(line)) {
          issues.push(`${location} ${pattern.label} outside i18n/message-key owners`);
        }
      });
    });
  });
}

TARGETS.forEach(walk);

if (issues.length) {
  console.log(["FAIL raw-ui-text-gate", ...issues.map((issue) => `- ${issue}`)].join("\n"));
  process.exit(1);
}

console.log("PASS raw-ui-text-gate");
