const fs = require("node:fs");
const path = require("node:path");
const { spawnSync } = require("node:child_process");

const ROOT = process.cwd();
const DIST_DIR = path.join(ROOT, "dist");
const APP_NAMESPACE = "PRODUCTION_CONTROL_CHECKLIST";
const COMPONENT_PRELOAD = path.join(DIST_DIR, "Component-preload.js");
const BUILD_INFO = path.join(DIST_DIR, "build-info.json");
const ALLOWED_EXTENSIONS = new Set([".js", ".xml", ".json", ".properties", ".html"]);

function runSapBuild() {
  const result = spawnSync("ui5", ["build", "preload", "--config", "ui5.yaml", "--clean-dest", "--dest", "dist"], {
    stdio: "inherit",
    shell: true
  });
  if (result.error) {
    throw result.error;
  }
  process.exit(typeof result.status === "number" ? result.status : 1);
}

function walk(dirPath, collected) {
  fs.readdirSync(dirPath, { withFileTypes: true }).forEach((entry) => {
    const fullPath = path.join(dirPath, entry.name);
    if (entry.isDirectory()) {
      if (entry.name === "test") {
        return;
      }
      walk(fullPath, collected);
      return;
    }
    if (ALLOWED_EXTENSIONS.has(path.extname(entry.name).toLowerCase()) && entry.name !== "Component-preload.js") {
      collected.push(fullPath);
    }
  });
}

function moduleName(filePath) {
  return path.relative(DIST_DIR, filePath).split(path.sep).join("/");
}

function readModuleSource(filePath) {
  let iAttempt = 0;
  while (iAttempt < 3) {
    try {
      return fs.readFileSync(filePath, "utf8");
    } catch (oError) {
      if (!oError || (oError.code !== "ENOENT" && oError.code !== "EPERM")) {
        throw oError;
      }
    }
    iAttempt += 1;
  }
  return null;
}

function jsString(value) {
  return JSON.stringify(String(value))
    .replace(/\u2028/g, "\\u2028")
    .replace(/\u2029/g, "\\u2029");
}

function writeBuildInfo(mode, note) {
  const payload = {
    mode,
    generatedAt: new Date().toISOString(),
    note
  };
  fs.writeFileSync(BUILD_INFO, JSON.stringify(payload, null, 2));
}

function buildLocalPreload() {
  if (!fs.existsSync(DIST_DIR)) {
    throw new Error("dist directory does not exist. Run build:dist first.");
  }
  const files = [];
  walk(DIST_DIR, files);
  const modules = files
    .sort()
    .map((filePath) => {
      const sSource = readModuleSource(filePath);
      return sSource === null
        ? null
        : `${jsString(moduleName(filePath))}:${jsString(sSource)}`;
    })
    .filter(Boolean)
    .join(",\n");
  const payload = [
    "sap.ui.require.preload({",
    modules,
    `}, ${jsString(`${APP_NAMESPACE}/Component-preload`)});`,
    ""
  ].join("\n");
  fs.writeFileSync(COMPONENT_PRELOAD, payload, "utf8");
  writeBuildInfo(
    "local-preload",
    "Component-preload.js generated from local dist sources because SAPUI5 framework metadata is not resolvable in this environment."
  );
  console.log([
    "build:preload local mode: generated dist/Component-preload.js from local sources.",
    "Use `npm run build:preload:sap` with SAPUI5 registry access, or set `PCCT_ENFORCE_SAPUI5_BUILD=1` to enforce the official build."
  ].join("\n"));
}

if (process.env.PCCT_ENFORCE_SAPUI5_BUILD === "1") {
  runSapBuild();
} else {
  buildLocalPreload();
}
