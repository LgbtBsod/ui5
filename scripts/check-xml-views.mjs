import fs from "fs";

function fail(message) {
  console.error(`XML architecture check failed: ${message}`);
  process.exit(1);
}

function read(filePath) {
  return fs.readFileSync(filePath, "utf8");
}

function measure(xml) {
  return {
    tags: (xml.match(/<(?!!|\?|\/)([A-Za-z0-9:_-]+)/g) || []).length,
    vbox: (xml.match(/<VBox\b/g) || []).length,
    hbox: (xml.match(/<HBox\b/g) || []).length,
    fragments: (xml.match(/<core:Fragment\b/g) || []).length
  };
}

function assertMetrics(filePath, limits) {
  const stats = measure(read(filePath));
  for (const [key, limit] of Object.entries(limits)) {
    if (stats[key] > limit) {
      fail(`${filePath} exceeds DOM metric ${key}: ${stats[key]} > ${limit}.`);
    }
  }
  console.log(`${filePath}: ${JSON.stringify(stats)}`);
}

function assertIncludes(filePath, pattern, message) {
  const text = read(filePath);
  if (!pattern.test(text)) {
    fail(`${filePath}: ${message}`);
  }
}

const appView = read("view/App.view.xml");
const searchView = read("view/Search.view.xml");
const detailView = read("view/Detail.view.xml");
const checksExpandedDialog = read("view/fragment/ChecksExpandedDialog.fragment.xml");
const barriersExpandedDialog = read("view/fragment/BarriersExpandedDialog.fragment.xml");
const workflowAnalyticsDialog = read("view/fragment/WorkflowAnalyticsDialog.fragment.xml");
const workflowAnalyticsTopline = read("view/fragment/WorkflowAnalyticsTopline.fragment.xml");
const workflowAnalyticsBreakdowns = read("view/fragment/WorkflowAnalyticsBreakdowns.fragment.xml");
const locationValueHelpDialog = read("view/fragment/LocationValueHelpDialog.fragment.xml");
const lockSwitchStatus = read("view/fragment/LockSwitchStatus.fragment.xml");

if (!/class="rnvApp"/.test(appView)) {
  fail("App.view.xml must declare class=\"rnvApp\" on the root view.");
}

if (/WorkflowAnalyticsDialog/.test(searchView)) {
  fail("Search.view.xml must not inline WorkflowAnalyticsDialog fragment.");
}

for (const fragmentName of [
  "LocationValueHelpDialog",
  "ChecksExpandedDialog",
  "BarriersExpandedDialog"
]) {
  if (detailView.includes(fragmentName)) {
    fail(`Detail.view.xml must not inline ${fragmentName}.`);
  }
}

if (!/busyIndicatorDelay="0"/.test(searchView) || !/id="searchSmartFilterBar"/.test(searchView)) {
  fail("Search.view.xml must keep busyIndicatorDelay=\"0\" on SmartFilterBar initialization state.");
}

if (!/items="\{path:'view>\/infoCards', factory: '\.infoCardFactory'\}"/.test(detailView)) {
  fail("Detail.view.xml must use infoCardFactory for infoCards GridList.");
}

if (!/<t:Table[\s\S]*rows="\{selected>\/checks\}"/.test(detailView) || !/<Table[\s\S]*items="\{selected>\/checks\}"/.test(detailView)) {
  fail("Detail.view.xml must keep dual-table contract for checks (t:Table + phone fallback Table).");
}

if (!/<t:Table[\s\S]*rows="\{selected>\/barriers\}"/.test(detailView) || !/<Table[\s\S]*items="\{selected>\/barriers\}"/.test(detailView)) {
  fail("Detail.view.xml must keep dual-table contract for barriers (t:Table + phone fallback Table).");
}

assertIncludes("view/fragment/WorkflowAnalyticsDialog.fragment.xml", /busyIndicatorDelay="0"/, "Workflow analytics dialog must keep busyIndicatorDelay=\"0\".");
assertIncludes("view/fragment/LocationValueHelpDialog.fragment.xml", /busyIndicatorDelay="0"/, "Location value help must keep busyIndicatorDelay=\"0\".");
assertIncludes("view/fragment/ChecksExpandedDialog.fragment.xml", /busyIndicatorDelay="0"/, "Checks expanded dialog must keep busyIndicatorDelay=\"0\".");
assertIncludes("view/fragment/BarriersExpandedDialog.fragment.xml", /busyIndicatorDelay="0"/, "Barriers expanded dialog must keep busyIndicatorDelay=\"0\".");
assertIncludes("view/fragment/LockSwitchStatus.fragment.xml", /busyIndicatorDelay="0"/, "Lock switch must keep busyIndicatorDelay=\"0\".");

if (!/<t:Table[\s\S]*rows="\{selected>\/checks\}"/.test(checksExpandedDialog) || !/<Table[\s\S]*items="\{selected>\/checks\}"/.test(checksExpandedDialog)) {
  fail("ChecksExpandedDialog.fragment.xml must keep dual-table contract.");
}

if (!/<t:Table[\s\S]*rows="\{selected>\/barriers\}"/.test(barriersExpandedDialog) || !/<Table[\s\S]*items="\{selected>\/barriers\}"/.test(barriersExpandedDialog)) {
  fail("BarriersExpandedDialog.fragment.xml must keep dual-table contract.");
}

assertMetrics("view/App.view.xml", { tags: 20, vbox: 2, hbox: 1, fragments: 0 });
assertMetrics("view/Search.view.xml", { tags: 130, vbox: 26, hbox: 8, fragments: 3 });
assertMetrics("view/Detail.view.xml", { tags: 300, vbox: 46, hbox: 22, fragments: 6 });
assertMetrics("view/fragment/WorkflowAnalyticsDialog.fragment.xml", { tags: 28, vbox: 3, hbox: 1, fragments: 2 });
assertMetrics("view/fragment/WorkflowAnalyticsTopline.fragment.xml", { tags: 58, vbox: 8, hbox: 9, fragments: 0 });
assertMetrics("view/fragment/WorkflowAnalyticsBreakdowns.fragment.xml", { tags: 70, vbox: 22, hbox: 5, fragments: 0 });
assertMetrics("view/fragment/DetailControlRail.fragment.xml", { tags: 80, vbox: 4, hbox: 3, fragments: 1 });
assertMetrics("view/fragment/SearchLoadStatePanel.fragment.xml", { tags: 16, vbox: 3, hbox: 3, fragments: 0 });
assertMetrics("view/fragment/ChecksExpandedDialog.fragment.xml", { tags: 55, vbox: 2, hbox: 1, fragments: 0 });
assertMetrics("view/fragment/BarriersExpandedDialog.fragment.xml", { tags: 55, vbox: 2, hbox: 1, fragments: 0 });

console.log("XML architecture check passed.");
