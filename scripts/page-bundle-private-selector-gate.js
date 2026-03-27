#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const ROOT = process.cwd();
const FILES = [
  "app/styles/modules/40_page_search.css",
  "app/styles/modules/41_page_detail.css"
];
const PATTERNS = [
  { regex: /\.sapUiCompFilterBarToolbar\b/g, message: "SmartFilterBar renderer selectors must stay out of page bundles" },
  { regex: /\.sapUiTable(?:Cnt|CtrlScr|CCnt)?\b/g, message: "sap.ui.table renderer selectors must stay out of page bundles" },
  { regex: /\.sapUxAPObjectPage[A-Za-z-]*\b/g, message: "ObjectPage renderer selectors must stay out of page bundles" },
  { regex: /\.sapMSwtCont\b/g, message: "Switch renderer container selectors must stay out of page bundles" }
];

function lineFromIndex(source, index) {
  return source.slice(0, index).split(/\r?\n/).length;
}

const issues = [];

FILES.forEach((relPath) => {
  const fullPath = path.join(ROOT, relPath);
  const source = fs.readFileSync(fullPath, "utf8");
  PATTERNS.forEach((pattern) => {
    let match;
    while ((match = pattern.regex.exec(source))) {
      issues.push(`${relPath}:${lineFromIndex(source, match.index)} ${pattern.message}`);
    }
  });
});

if (issues.length) {
  console.log(["FAIL page-bundle-private-selector-gate", ...issues.map((issue) => `- ${issue}`)].join("\n"));
  process.exit(1);
}

console.log("PASS page-bundle-private-selector-gate");
