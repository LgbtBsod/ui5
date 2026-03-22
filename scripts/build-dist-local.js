const fs = require("node:fs");
const path = require("node:path");
const { spawnSync } = require("node:child_process");

const ROOT = process.cwd();
const APP_DIR = path.join(ROOT, "app");
const DIST_DIR = path.join(ROOT, "dist");

function rimraf(targetPath) {
  fs.rmSync(targetPath, { recursive: true, force: true });
}

function copyRecursive(sourcePath, targetPath) {
  const stats = fs.statSync(sourcePath);
  if (stats.isDirectory()) {
    fs.mkdirSync(targetPath, { recursive: true });
    fs.readdirSync(sourcePath, { withFileTypes: true }).forEach((entry) => {
      if (entry.name === "test") {
        return;
      }
      copyRecursive(path.join(sourcePath, entry.name), path.join(targetPath, entry.name));
    });
    return;
  }
  fs.mkdirSync(path.dirname(targetPath), { recursive: true });
  fs.copyFileSync(sourcePath, targetPath);
}

function tryUi5Build() {
  const result = process.platform === "win32"
    ? spawnSync("cmd.exe", ["/d", "/s", "/c", "ui5 build --config ui5.yaml --clean-dest --dest dist"], {
        encoding: "utf8"
      })
    : spawnSync("ui5", ["build", "--config", "ui5.yaml", "--clean-dest", "--dest", "dist"], {
        encoding: "utf8"
      });
  return result;
}

function shouldFallback(result) {
  const stderr = String(result && result.stderr ? result.stderr : "");
  const stdout = String(result && result.stdout ? result.stdout : "");
  const combined = stderr + "\n" + stdout;
  return combined.includes("@sapui5/distribution-metadata")
    || combined.includes("framework package")
    || combined.includes("Unable to resolve")
    || combined.includes("'ui5' is not recognized")
    || combined.includes("not recognized as an internal or external command")
    || combined.includes("ENOENT");
}

function writeBuildInfo(mode) {
  const payload = {
    mode,
    generatedAt: new Date().toISOString(),
    note: mode === "fallback-copy"
      ? "Static dist prepared without UI5 CLI build because SAPUI5 framework metadata was unavailable in the current environment."
      : "Dist built with UI5 CLI."
  };
  fs.writeFileSync(path.join(DIST_DIR, "build-info.json"), JSON.stringify(payload, null, 2));
}

const ui5Result = tryUi5Build();
if (ui5Result.status === 0) {
  if (ui5Result.stdout) {
    process.stdout.write(ui5Result.stdout);
  }
  if (ui5Result.stderr) {
    process.stderr.write(ui5Result.stderr);
  }
  writeBuildInfo("ui5-cli");
  process.exit(0);
}

if (ui5Result.error) {
  console.warn("ui5 build invocation failed, switching to static dist fallback.");
  console.warn(String(ui5Result.error.message || ui5Result.error));
} else if (!shouldFallback(ui5Result)) {
  if (ui5Result.stdout) {
    process.stdout.write(ui5Result.stdout);
  }
  if (ui5Result.stderr) {
    process.stderr.write(ui5Result.stderr);
  }
  process.exit(typeof ui5Result.status === "number" ? ui5Result.status : 1);
}

rimraf(DIST_DIR);
copyRecursive(APP_DIR, DIST_DIR);
writeBuildInfo("fallback-copy");
console.log([
  "build:dist local fallback prepared ./dist from ./app.",
  "UI5 CLI build was skipped because SAPUI5 framework metadata is not resolvable in this environment."
].join("\n"));
