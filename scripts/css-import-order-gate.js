const fs = require("fs");
const path = require("path");

const appStylesPath = path.join(__dirname, "..", "app", "styles", "app-styles.css");
const expectedOrder = [
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

const content = fs.readFileSync(appStylesPath, "utf8");
const actualOrder = content
  .split(/\r?\n/)
  .map((line) => line.trim())
  .filter((line) => line.startsWith("@import url("));

if (actualOrder.length !== expectedOrder.length || actualOrder.some((line, index) => line !== expectedOrder[index])) {
  console.error("ERROR: app-styles.css import order drift detected.");
  console.error("Expected:");
  expectedOrder.forEach((line) => console.error(`  ${line}`));
  console.error("Actual:");
  actualOrder.forEach((line) => console.error(`  ${line}`));
  process.exit(1);
}

console.log("OK: CSS import order is stable");
