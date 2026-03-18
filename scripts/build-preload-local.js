const { spawnSync } = require("node:child_process");

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

if (process.env.PCCT_ENFORCE_SAPUI5_BUILD === "1") {
  runSapBuild();
}

console.log(
  [
    "build:preload local mode: skipped SAPUI5 preload build.",
    "Reason: SAPUI5 1.71.70 framework artifacts are not resolvable from public npm in this environment.",
    "Use `npm run build:preload:sap` with SAPUI5 registry access, or set `PCCT_ENFORCE_SAPUI5_BUILD=1` to enforce the build."
  ].join("\n")
);
