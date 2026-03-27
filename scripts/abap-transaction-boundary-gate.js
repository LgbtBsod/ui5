#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const ROOT = process.cwd();
const BACKEND_DIR = path.join(ROOT, "backend", "sap_backend", "src");
const CONFIG = JSON.parse(
  fs.readFileSync(path.join(ROOT, "scripts", "abap-transaction-boundary-allowlist.json"), "utf8")
);

function read(relPath) {
  return fs.readFileSync(path.join(BACKEND_DIR, relPath), "utf8");
}

function lineFromIndex(source, index) {
  return source.slice(0, index).split(/\r?\n/).length;
}

function findAll(source, regex) {
  const results = [];
  let match;
  while ((match = regex.exec(source))) {
    results.push(match);
  }
  return results;
}

function isCommentLine(source, index) {
  const lineStart = source.lastIndexOf("\n", index);
  const start = lineStart < 0 ? 0 : lineStart + 1;
  const line = source.slice(start, source.indexOf("\n", index) < 0 ? source.length : source.indexOf("\n", index));
  return /^\s*"/.test(line);
}

function pushViolation(violations, relPath, source, index, message) {
  violations.push({
    file: relPath,
    line: lineFromIndex(source, index),
    message
  });
}

function scanMethodBody(source, methodName) {
  const escaped = methodName.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const regex = new RegExp(`METHOD\\s+${escaped}\\.[\\s\\S]*?ENDMETHOD\\.`, "i");
  const match = source.match(regex);
  return match ? { body: match[0], index: match.index || 0 } : null;
}

(function main() {
  const violations = [];
  const commitRegex = /\bCOMMIT\s+WORK(?:\s+AND\s+WAIT)?\b/gi;
  const files = fs.readdirSync(BACKEND_DIR).filter((name) => /\.abap$/i.test(name));
  const allowedCommitOwners = new Set(CONFIG.allowedCommitOwners || []);
  const forbiddenFacadeOwners = new Set(CONFIG.forbiddenFacadeOwners || []);

  files.forEach((file) => {
    const source = read(file);
    findAll(source, commitRegex).forEach((match) => {
      if (isCommentLine(source, match.index)) {
        return;
      }
      if (forbiddenFacadeOwners.has(file)) {
        pushViolation(
          violations,
          file,
          source,
          match.index,
          "Gateway facade metadata/runtime classes must not issue COMMIT WORK directly"
        );
        return;
      }
      if (!allowedCommitOwners.has(file)) {
        pushViolation(
          violations,
          file,
          source,
          match.index,
          "COMMIT WORK owner is not allowlisted for backend LUW governance"
        );
      }
    });
  });

  const lockManagerFile = "zcl_zodata_lock_manager.clas.abap";
  const lockManagerSource = read(lockManagerFile);
  const statusMethod = scanMethodBody(lockManagerSource, "zif_zodata_lock_manager~status");
  if (statusMethod && /SELECT\s+SINGLE[\s\S]*?FROM\s+ztodata_hdr/i.test(statusMethod.body)) {
    pushViolation(
      violations,
      lockManagerFile,
      lockManagerSource,
      statusMethod.index,
      "Lock status must use canonical lock-layer helpers; direct ztodata_hdr SQL inside zif_zodata_lock_manager~status is forbidden"
    );
  }

  if (violations.length) {
    console.error("abap-transaction-boundary-gate FAILED");
    violations.forEach((violation) => {
      console.error(` - ${violation.file}:${violation.line} ${violation.message}`);
    });
    process.exit(1);
  }

  console.log("abap-transaction-boundary-gate PASS");
})();
