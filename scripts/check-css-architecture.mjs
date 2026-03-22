import fs from "fs";
import path from "path";

const runtimeRoot = fs.existsSync(path.join(process.cwd(), "app", "styles")) ? "app" : "";
const entryCandidates = [
  path.join(runtimeRoot, "styles/app-styles.css").replace(/\\/g, "/"),
  path.join(runtimeRoot, "styles/source-entry.css").replace(/\\/g, "/")
];
const entryPath = entryCandidates.find((candidate) => fs.existsSync(candidate)) || "";
const forbiddenLegacyFiles = [
  path.join(runtimeRoot, "styles/modules/style-core.css").replace(/\\/g, "/"),
  path.join(runtimeRoot, "styles/modules/style-components.css").replace(/\\/g, "/"),
  path.join(runtimeRoot, "styles/modules/style-overrides.css").replace(/\\/g, "/")
];
const expectedImports = [
  '@import url("./modules/00_tokens.css");',
  '@import url("./modules/01_theme-modes.css");',
  '@import url("./modules/02_background.css");',
  '@import url("./modules/10_base.css");',
  '@import url("./modules/20_surface.css");',
  '@import url("./modules/21_controls.css");',
  '@import url("./modules/22_skeleton.css");',
  '@import url("./modules/23_dialogs.css");',
  '@import url("./modules/24_table_common.css");',
  '@import url("./modules/40_page_search.css");',
  '@import url("./modules/41_page_detail.css");',
  '@import url("./modules/42_page_analytics.css");',
  '@import url("./modules/90_ui5_overrides.css");'
];
const modules = [
  path.join(runtimeRoot, "styles/modules/00_tokens.css").replace(/\\/g, "/"),
  path.join(runtimeRoot, "styles/modules/01_theme-modes.css").replace(/\\/g, "/"),
  path.join(runtimeRoot, "styles/modules/02_background.css").replace(/\\/g, "/"),
  path.join(runtimeRoot, "styles/modules/10_base.css").replace(/\\/g, "/"),
  path.join(runtimeRoot, "styles/modules/20_surface.css").replace(/\\/g, "/"),
  path.join(runtimeRoot, "styles/modules/21_controls.css").replace(/\\/g, "/"),
  path.join(runtimeRoot, "styles/modules/22_skeleton.css").replace(/\\/g, "/"),
  path.join(runtimeRoot, "styles/modules/23_dialogs.css").replace(/\\/g, "/"),
  path.join(runtimeRoot, "styles/modules/24_table_common.css").replace(/\\/g, "/"),
  path.join(runtimeRoot, "styles/modules/40_page_search.css").replace(/\\/g, "/"),
  path.join(runtimeRoot, "styles/modules/41_page_detail.css").replace(/\\/g, "/"),
  path.join(runtimeRoot, "styles/modules/42_page_analytics.css").replace(/\\/g, "/"),
  path.join(runtimeRoot, "styles/modules/90_ui5_overrides.css").replace(/\\/g, "/")
];
const patchFiles = new Set([
  path.join(runtimeRoot, "styles/modules/90_ui5_overrides.css").replace(/\\/g, "/")
]);

function fail(message) {
  console.error(`CSS architecture check failed: ${message}`);
  process.exit(1);
}

function assertPatchDiscipline(filePath, css) {
  const lines = css.split(/\r?\n/);
  const importantLines = [];
  lines.forEach((line, index) => {
    if (line.includes("!important")) {
      importantLines.push(index);
    }
  });
  if (lines.length > 70) {
    fail(`${filePath} must stay small; expected <= 70 lines, got ${lines.length}.`);
  }
  for (const index of importantLines) {
    const current = lines[index];
    const previous = lines[index - 1] || "";
    const hasComment = /\/\*/.test(current) || /\/\*/.test(previous);
    if (!hasComment) {
      fail(`${filePath} has !important without an explanatory comment near line ${index + 1}.`);
    }
  }
}

function stripCssComments(css) {
  return String(css || "").replace(/\/\*[\s\S]*?\*\//g, "");
}

if (entryPath) {
  const entry = fs.readFileSync(entryPath, "utf8").trim();
  if (entry !== expectedImports.join("\n")) {
    fail(`${entryPath} must be entry-only with the exact module import order.`);
  }
}

for (const legacyFile of forbiddenLegacyFiles) {
  if (fs.existsSync(legacyFile)) {
    fail(`${legacyFile} must not exist in the active styles/modules tree; move it to hidden archive or remove it.`);
  }
}

for (const file of modules) {
  const css = fs.readFileSync(file, "utf8");
  const cssWithoutComments = stripCssComments(css);
  if (patchFiles.has(file)) {
    assertPatchDiscipline(file, css);
  } else if (css.includes("!important")) {
    fail(`${file} contains !important outside 90_ui5_overrides.css.`);
  }
  if (/outline\s*:\s*none/i.test(css) && !patchFiles.has(file)) {
    fail(`${file} contains outline:none outside patches.`);
  }
  if (css.includes("__view")) {
    fail(`${file} contains forbidden __view selectors.`);
  }
  const offenders = [...cssWithoutComments.matchAll(/([^{}]+)\{/g)]
    .map((match) => match[1].trim())
    .filter((selector) => selector.includes(".sap"))
    .filter((selector) => !selector.startsWith("@"))
    .filter((selector) => selector.split(",").some((part) => {
      const normalized = part.trim().replace(/\s+/g, " ");
      return !normalized.includes(".chkApp ")
        && !normalized.includes(".chkAppRoot")
        && !normalized.includes("#ui5_container.chkAppRoot")
        && !normalized.includes("body.chkAppRoot")
        && !normalized.includes("html.chkAppRoot");
    }));
  if (offenders.length) {
    fail(`${file} has unscoped .sap* selectors: ${offenders.slice(0, 3).join(", ")}`);
  }
}

console.log("CSS architecture check passed.");
