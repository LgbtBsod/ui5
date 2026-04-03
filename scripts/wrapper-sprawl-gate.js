#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const ROOT = process.cwd();
const bannedFiles = [
  "app/service/framework/ControllerRouteRuntime.js",
  "app/service/framework/FeedbackCoordinator.js",
  "app/controller/search/SearchCommandPolicy.js",
  "app/controller/detail/DetailCommandPolicy.js"
];
const scanDirs = [
  path.join(ROOT, "app", "controller"),
  path.join(ROOT, "app", "service", "framework"),
  path.join(ROOT, "app", "service", "runtime", "component")
];
const issues = [];

function walk(dir, onFile) {
  if (!fs.existsSync(dir)) {
    return;
  }
  fs.readdirSync(dir, { withFileTypes: true }).forEach((entry) => {
    const fullPath = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      walk(fullPath, onFile);
      return;
    }
    if (entry.name.endsWith(".js")) {
      onFile(fullPath);
    }
  });
}

function relative(filePath) {
  return path.relative(ROOT, filePath).replace(/\\/g, "/");
}

function detectThinWrapper(fullPath) {
  const text = fs.readFileSync(fullPath, "utf8");
  const relPath = relative(fullPath);
  const forwardedCalls = (text.match(/return\s+[A-Za-z0-9_$\.]+\.[A-Za-z0-9_$]+\(/g) || []).length;
  const localHelpers = (text.match(/function\s+[A-Za-z0-9_$]+\s*\(/g) || []).length;
  const exportedActions = (text.match(/:\s*function\s*\(/g) || []).length;
  const hasRuntimeLogic = /Promise\.all|Object\.assign|ModelStateRuntime|Effects\.|GatewayClient|ResizeObserver|classList|querySelector|switch\s*\(|if\s*\(/.test(text);

  if (hasRuntimeLogic) {
    return;
  }
  if (forwardedCalls >= 3 && localHelpers <= 1 && exportedActions >= 3) {
    issues.push(`${relPath} behaves like a pass-through wrapper and should be merged or deleted`);
  }
}

bannedFiles.forEach((relativePath) => {
  const fullPath = path.join(ROOT, relativePath);
  if (fs.existsSync(fullPath)) {
    issues.push(`${relativePath} is a banned thin-wrapper owner`);
  }
});

[
  "app/Component.js",
  "app/service/framework/ComponentBootstrap.js",
  "app/service/runtime/component/ComponentLifecycleRuntime.js",
  "app/infra/adapters/ODataChecklistRepoAdapter.js"
].forEach((relativePath) => {
  const fullPath = path.join(ROOT, relativePath);
  if (!fs.existsSync(fullPath)) {
    issues.push(`${relativePath} missing; bootstrap/runtime ownership map is incomplete`);
  }
});

scanDirs.forEach((dir) => {
  walk(dir, detectThinWrapper);
});

if (issues.length) {
  console.log(["FAIL wrapper-sprawl-gate", ...issues.map((issue) => `- ${issue}`)].join("\n"));
  process.exit(1);
}

console.log("PASS wrapper-sprawl-gate");
