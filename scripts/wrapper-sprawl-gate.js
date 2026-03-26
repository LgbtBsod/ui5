#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const ROOT = process.cwd();
const bannedFiles = [
  "app/controller/search/SearchCommandPolicy.js",
  "app/service/framework/ControllerRouteRuntime.js",
  "app/service/framework/FeedbackCoordinator.js"
];
const issues = [];

bannedFiles.forEach((relativePath) => {
  const fullPath = path.join(ROOT, relativePath);
  if (fs.existsSync(fullPath)) {
    issues.push(`${relativePath} is a banned thin-wrapper owner`);
  }
});

if (issues.length) {
  console.log(["FAIL wrapper-sprawl-gate", ...issues.map((issue) => `- ${issue}`)].join("\n"));
  process.exit(1);
}

console.log("PASS wrapper-sprawl-gate");
