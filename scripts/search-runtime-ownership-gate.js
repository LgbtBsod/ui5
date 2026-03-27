#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const ROOT = process.cwd();

function read(relPath) {
  return fs.readFileSync(path.join(ROOT, relPath), "utf8");
}

const issues = [];
const viewportFile = "app/service/features/search/runtime/SearchViewportRuntime.js";
const smartTableBehaviorFile = "app/controller/search/SearchSmartTableBehavior.js";
const viewportSource = read(viewportFile);
const smartTableBehaviorSource = read(smartTableBehaviorFile);

if (/configureSearchResultTable\s*\(/.test(viewportSource)) {
  issues.push(`${viewportFile}: viewport runtime must not own search table configuration`);
}

if (!/configureSearchResultTable\s*\(/.test(smartTableBehaviorSource)) {
  issues.push(`${smartTableBehaviorFile}: smart table behavior must remain the owner of search table configuration`);
}

if (issues.length) {
  console.log(["FAIL search-runtime-ownership-gate", ...issues.map((issue) => `- ${issue}`)].join("\n"));
  process.exit(1);
}

console.log("PASS search-runtime-ownership-gate");
