#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const ROOT = process.cwd();
const TARGETS = [
  path.join(ROOT, "app"),
  path.join(ROOT, "backend"),
  path.join(ROOT, "scripts")
];
const FILE_EXTENSIONS = /\.(js|xml|py|abap|md|sh)$/i;
const issues = [];
const allowlist = new Set([
  "backend/mock_gateway/api/gateway_canonical_api.py",
  "backend/mock_gateway/tests/test_gateway_contract_frontend_aliases.py",
  "backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap",
  "backend/mock_gateway/README_ODATA.md",
  "docs/ui-runtime-audit-latest.json",
  "docs/artifacts/gateway-browser-attachment-dirty-report.json",
  "docs/artifacts/gateway-only-smoke-report.json"
]);

function relative(filePath) {
  return path.relative(ROOT, filePath).replace(/\\/g, "/");
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
      allowlist.has(relPath)
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
      if (/\bObjectUuid\b|\bobjectUuid\b/.test(lineText)) {
        issues.push(`${relPath}:${index + 1} legacy ObjectUuid alias leaked outside boundary compatibility ingress`);
      }
      if (/\bRootKey\b|\bRootId\b/.test(lineText)) {
        issues.push(`${relPath}:${index + 1} legacy RootKey/RootId alias leaked outside boundary compatibility ingress`);
      }
    });
  });
}

TARGETS.forEach(walk);

if (issues.length) {
  console.log(["FAIL legacy-alias-leakage-gate", ...issues.map((issue) => `- ${issue}`)].join("\n"));
  process.exit(1);
}

console.log("PASS legacy-alias-leakage-gate");
