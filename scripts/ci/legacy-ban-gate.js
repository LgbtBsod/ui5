#!/usr/bin/env node

/**
 * Legacy Ban Gate
 *
 * Hard-fails CI if any legacy transport/facade artifacts exist or are referenced.
 * This protects the architecture during migration to real SAP Gateway.
 */

const fs = require("fs");
const path = require("path");

const ROOT = process.cwd();
const RUNTIME_ROOT = ".";

function exists(p) {
  try {
    fs.accessSync(p);
    return true;
  } catch {
    return false;
  }
}

function walk(dir, out = []) {
  const entries = fs.readdirSync(dir, { withFileTypes: true });
  for (const e of entries) {
    if (e.name === "node_modules" || e.name === ".git") continue;
    const full = path.join(dir, e.name);
    if (e.isDirectory()) walk(full, out);
    else if (e.isFile() && /\.(js|xml|json)$/.test(e.name)) out.push(full);
  }
  return out;
}

function makeErr(ruleId, file, line, message, fixHint) {
  return {
    ruleId,
    file: file.replace(/\\/g, "/"),
    line,
    message,
    fixHint,
    goodExample: "Use GatewayClient -> GatewayODataClient -> infra/adapters -> ports -> service/domain",
    badExample: "Import/use legacy BackendAdapter/core transports/legacy facades",
  };
}

function lineOf(text, idx) {
  let line = 1;
  for (let i = 0; i < idx; i += 1) {
    if (text.charCodeAt(i) === 10) line += 1;
  }
  return line;
}

function bannedArtifactPaths() {
  return [
    "service/backend/BackendAdapter.js",
    "service/backend/SharedBackendServiceCore.js",
    "service/backend/core",
    "service/search/SearchFacade.js",
    "service/detail/DetailFacade.js",
  ].map((p) => path.join(ROOT, RUNTIME_ROOT, p));
}

function scanRoots() {
  return ["controller", "service", "infra", "manager", "util", "Component.js", "manifest.json"]
    .map((p) => path.join(ROOT, RUNTIME_ROOT, p))
    .filter(exists);
}

function bannedSnippets() {
  return [
    "service/backend/BackendAdapter",
    "sap_ui5/service/backend/BackendAdapter",
    "service/backend/core/",
    "service/search/SearchFacade",
    "service/detail/DetailFacade",
  ];
}

function collectFiles(roots) {
  const files = [];
  for (const root of roots) {
    const stat = fs.statSync(root);
    if (stat.isFile()) files.push(root);
    else files.push(...walk(root));
  }
  return files;
}

function collectArtifactErrors() {
  const errors = [];
  for (const p of bannedArtifactPaths()) {
    if (exists(p)) {
      errors.push(
        makeErr(
          "legacy-ban",
          p,
          1,
          `Legacy artifact must not exist: ${p}`,
          "Delete it. Replace with ports/adapters in infra/* and facades in service/domain/*"
        )
      );
    }
  }
  return errors;
}

function collectReferenceErrors(files) {
  const errors = [];
  const snippets = bannedSnippets();
  for (const f of files) {
    const src = fs.readFileSync(f, "utf8");
    for (const s of snippets) {
      const idx = src.indexOf(s);
      if (idx >= 0) {
        errors.push(
          makeErr(
            "legacy-ban",
            f,
            lineOf(src, idx),
            `Banned legacy reference found: ${s}`,
            "Remove reference. Use GatewayODataClient-based adapters via CtxFactory."
          )
        );
      }
    }
  }
  return errors;
}

function main() {
  const files = collectFiles(scanRoots());
  const errors = [...collectArtifactErrors(), ...collectReferenceErrors(files)];

  if (errors.length) {
    console.log(JSON.stringify({ ok: false, errors }, null, 2));
    process.exit(2);
  }

  console.log(JSON.stringify({ ok: true, errors: [] }, null, 2));
}

main();
