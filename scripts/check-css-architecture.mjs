import fs from "fs";
import path from "path";

const styleRoot = fs.existsSync(path.join(process.cwd(), "app", "styles")) ? path.join("app", "styles") : "styles";
const styleModuleRoot = path.join(styleRoot, "modules").replace(/\\/g, "/");
const entryCandidates = [
  path.join(styleRoot, "app-styles.css").replace(/\\/g, "/"),
  path.join(styleRoot, "style.css").replace(/\\/g, "/")
];
const entryPath = entryCandidates.find((candidate) => fs.existsSync(candidate)) || "";
const forbiddenLegacyFiles = [
  path.join(styleModuleRoot, "style-core.css").replace(/\\/g, "/"),
  path.join(styleModuleRoot, "style-components.css").replace(/\\/g, "/"),
  path.join(styleModuleRoot, "style-overrides.css").replace(/\\/g, "/")
];
const moduleNames = [
  "00_tokens.css",
  "01_theme-modes.css",
  "02_background.css",
  "10_base.css",
  "20_surface.css",
  "21_controls.css",
  "22_skeleton.css",
  "23_dialogs.css",
  "40_page_search.css",
  "41_page_detail.css",
  "42_page_analytics.css",
  "90_ui5_patches.css",
  "91_ui5_layout_patches.css",
  "92_ui5_surface_tuning.css"
];
const expectedImports = moduleNames.map((name) => `@import url("./modules/${name}");`);
const modules = moduleNames.map((name) => path.join(styleModuleRoot, name).replace(/\\/g, "/"));
const patchFiles = new Set([
  path.join(styleModuleRoot, "90_ui5_patches.css").replace(/\\/g, "/"),
  path.join(styleModuleRoot, "91_ui5_layout_patches.css").replace(/\\/g, "/"),
  path.join(styleModuleRoot, "92_ui5_surface_tuning.css").replace(/\\/g, "/")
]);

function fail(message) {
  console.error(`CSS architecture check failed: ${message}`);
  process.exit(1);
}

function assertExists(filePath) {
  if (!fs.existsSync(filePath)) {
    fail(`Missing required stylesheet module: ${filePath}`);
  }
}

function assertPatchDiscipline(filePath, css) {
  const lines = css.split(/\r?\n/);
  const importantLines = [];
  const maxLines = filePath.endsWith("92_ui5_surface_tuning.css") ? 180 : 70;
  lines.forEach((line, index) => {
    if (line.includes("!important")) {
      importantLines.push(index);
    }
  });
  if (lines.length > maxLines) {
    fail(`${filePath} must stay small; expected <= ${maxLines} lines, got ${lines.length}.`);
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
  assertExists(file);
  const css = fs.readFileSync(file, "utf8");
  const cssWithoutComments = stripCssComments(css);
  if (patchFiles.has(file)) {
    assertPatchDiscipline(file, css);
  } else if (css.includes("!important")) {
    fail(`${file} contains !important outside patch modules.`);
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
