#!/usr/bin/env node
const fs = require("fs");
const path = require("path");
const { detectRuntimeRoot } = require("./qa-shared");

const ROOT = process.cwd();
const RUNTIME_ROOT = detectRuntimeRoot(ROOT);
const SCAN_TARGETS = [
  "controller",
  "service",
  "infra",
  "util",
  "control",
  "view",
  "Component.js",
  "manifest.json"
];

const RULES = [
  { id: "fetch", regex: /fetch\s*\(/g },
  { id: "XMLHttpRequest", regex: /\bnew\s+XMLHttpRequest\s*\(/g },
  { id: "axios", regex: /\baxios\b/g },
  { id: "MockServer", regex: /sap\.ui\.core\.util\.MockServer/g },
  { id: "$expand|expand=", regex: /\$expand|expand=/g },
  { id: "routerClass.*FlexibleRouter", regex: /routerClass[\s\S]{0,200}?FlexibleRouter/g },
  { id: "setFclControlId", regex: /setFclControlId/g }
];

function normalize(relPath) {
  return relPath.replace(/\\/g, "/");
}

function collectFilesFromTarget(targetRelPath) {
  const absPath = path.join(ROOT, RUNTIME_ROOT, targetRelPath);
  if (!fs.existsSync(absPath)) {
    return [];
  }

  const stat = fs.statSync(absPath);
  if (stat.isFile()) {
    return [normalize(path.join(RUNTIME_ROOT, targetRelPath))];
  }

  const out = [];
  const entries = fs.readdirSync(absPath, { withFileTypes: true });
  for (const entry of entries) {
    if (entry.name === ".git" || entry.name === "node_modules" || entry.name === "scripts") {
      continue;
    }
    const nextTarget = path.join(targetRelPath, entry.name);
    if (entry.isDirectory()) {
      out.push(...collectFilesFromTarget(nextTarget));
    } else if (entry.isFile() && /\.(js|xml|json)$/.test(entry.name)) {
      out.push(normalize(path.join(RUNTIME_ROOT, nextTarget)));
    }
  }

  return out;
}

function lineNumberAt(source, index) {
  let line = 1;
  for (let i = 0; i < index; i += 1) {
    if (source.charCodeAt(i) === 10) {
      line += 1;
    }
  }
  return line;
}

function findRuleViolations(filePath, source) {
  const violations = [];
  for (const rule of RULES) {
    rule.regex.lastIndex = 0;
    let match;
    while ((match = rule.regex.exec(source)) !== null) {
      violations.push({
        rule: rule.id,
        file: filePath,
        line: lineNumberAt(source, match.index)
      });
      if (match[0].length === 0) {
        rule.regex.lastIndex += 1;
      }
    }
  }
  return violations;
}

function findCreateHashResetViolation(filePath, source) {
  if (!/replaceHash/.test(source) || !/__create/.test(source)) {
    return null;
  }
  const idx = source.indexOf("replaceHash");
  return {
    rule: "replaceHash + __create",
    file: filePath,
    line: lineNumberAt(source, idx >= 0 ? idx : 0)
  };
}

const files = [];
for (const target of SCAN_TARGETS) {
  files.push(...collectFilesFromTarget(target));
}

const uniqueFiles = [...new Set(files)].sort();
const allViolations = [];

for (const file of uniqueFiles) {
  const abs = path.join(ROOT, file);
  const source = fs.readFileSync(abs, "utf8");
  allViolations.push(...findRuleViolations(file, source));

  if (file.endsWith("/Component.js") || file === "Component.js" || file === "webapp/Component.js") {
    const extra = findCreateHashResetViolation(file, source);
    if (extra) {
      allViolations.push(extra);
    }
  }
}

if (allViolations.length > 0) {
  console.log("FAIL");
  for (const v of allViolations) {
    console.log(`- ${v.rule} :: ${v.file}:${v.line}`);
  }
  process.exit(1);
}

console.log("PASS");
process.exit(0);
