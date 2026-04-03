#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const ROOT = process.cwd();
const TARGETS = [
  path.join(ROOT, "app"),
  path.join(ROOT, "backend"),
  path.join(ROOT, "scripts")
];
const FILE_EXTENSIONS = /\.(js|py|abap|md)$/i;
const issues = [];
const allowlistPatterns = [
  /backend\/sap_backend\/src\/zcl_zodata_read_service\.clas\.abap$/,
  /backend\/sap_backend\/src\/zcl_zodata_dpc_ext\.clas\.abap$/,
  /backend\/mock_gateway\/api\/gateway_canonical_api\.py$/
];

function relative(filePath) {
  return path.relative(ROOT, filePath).replace(/\\/g, "/");
}

function isAllowed(relPath) {
  return allowlistPatterns.some((pattern) => pattern.test(relPath));
}

function walk(dir) {
  if (!fs.existsSync(dir)) {
    return;
  }
  fs.readdirSync(dir, { withFileTypes: true }).forEach((entry) => {
    const fullPath = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      walk(fullPath);
      return;
    }
    if (!FILE_EXTENSIONS.test(entry.name)) {
      return;
    }
    const relPath = relative(fullPath);
    if (
      isAllowed(relPath)
      || relPath.includes("__pycache__")
      || relPath.includes("/tests/")
      || /scripts\/.*-gate\.js$/.test(relPath)
    ) {
      return;
    }
    const lines = fs.readFileSync(fullPath, "utf8").split(/\r?\n/);
    lines.forEach((lineText, index) => {
      if (/^\s*(\/\*|\*|\/\/|#)/.test(lineText)) {
        return;
      }
      if (/\bsRootKey\b|\bsActiveRootKey\b|\bresolveRootKey\b/.test(lineText)) {
        issues.push(`${relPath}:${index + 1} legacy root-key helper naming must be replaced with dbKey semantics`);
      }
      if (/\broot_key\b/.test(lineText) && !/(["']root_key["']|root_key)\s*:/.test(lineText) && !/ATTACHMENT_BASE64_SAVE_PATH_FORBIDDEN/.test(lineText)) {
        issues.push(`${relPath}:${index + 1} internal naming still uses root_key outside transport boundary payloads`);
      }
    });
  });
}

TARGETS.forEach(walk);

if (issues.length) {
  console.log(["FAIL naming-debt-gate", ...issues.map((issue) => `- ${issue}`)].join("\n"));
  process.exit(1);
}

console.log("PASS naming-debt-gate");
