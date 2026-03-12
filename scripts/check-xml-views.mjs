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

const appView = read("app/view/App.view.xml");
const searchView = read("app/view/Search.view.xml");
const detailView = read("app/view/Detail.view.xml");
const analyticsView = read("app/view/Analytics.view.xml");
const checksExpandedDialog = read("app/view/fragment/ChecksExpandedDialog.fragment.xml");
const barriersExpandedDialog = read("app/view/fragment/BarriersExpandedDialog.fragment.xml");
const workflowAnalyticsTopline = read("app/view/fragment/WorkflowAnalyticsTopline.fragment.xml");
const workflowAnalyticsBreakdowns = read("app/view/fragment/WorkflowAnalyticsBreakdowns.fragment.xml");
const locationValueHelpDialog = read("app/view/fragment/LocationValueHelpDialog.fragment.xml");
const lockSwitchStatus = read("app/view/fragment/LockSwitchStatus.fragment.xml");

if (!/class="chkApp"/.test(appView)) {
  fail("app/view/App.view.xml must declare class=\"chkApp\" on the root view.");
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

if (!/id="searchSmartFilterBar"/.test(searchView) || !/visible="\{= \$\{view>\/smartFilterReady\} &amp;&amp; !\$\{view>\/bootstrapBusy\} \}"/.test(searchView)) {
  fail("Search.view.xml must keep SmartFilterBar gated by smartFilterReady and bootstrapBusy.");
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

assertIncludes("app/view/fragment/LocationValueHelpDialog.fragment.xml", /workflowBusyOverlayShellDialog/, "Location value help must keep workflow busy overlay shell.");
assertIncludes("app/view/fragment/ChecksExpandedDialog.fragment.xml", /workflowBusyOverlayShellDialog/, "Checks expanded dialog must keep workflow busy overlay shell.");
assertIncludes("app/view/fragment/BarriersExpandedDialog.fragment.xml", /workflowBusyOverlayShellDialog/, "Barriers expanded dialog must keep workflow busy overlay shell.");
assertIncludes("app/view/fragment/LockSwitchStatus.fragment.xml", /state>\/lockOperationPending/, "Lock switch must reflect lockOperationPending state.");
assertIncludes("app/view/Analytics.view.xml", /showNavButton="true"/, "Analytics.view.xml must keep route-level back navigation.");
assertIncludes("app/view/Analytics.view.xml", /analyticsRefreshButton/, "Analytics.view.xml must expose the analytics refresh action.");

if (!/<t:Table[\s\S]*rows="\{selected>\/checks\}"/.test(checksExpandedDialog) || !/<Table[\s\S]*items="\{selected>\/checks\}"/.test(checksExpandedDialog)) {
  fail("ChecksExpandedDialog.fragment.xml must keep dual-table contract.");
}

if (!/<t:Table[\s\S]*rows="\{selected>\/barriers\}"/.test(barriersExpandedDialog) || !/<Table[\s\S]*items="\{selected>\/barriers\}"/.test(barriersExpandedDialog)) {
  fail("BarriersExpandedDialog.fragment.xml must keep dual-table contract.");
}

assertMetrics("app/view/App.view.xml", { tags: 20, vbox: 3, hbox: 2, fragments: 0 });
assertMetrics("app/view/Search.view.xml", { tags: 155, vbox: 28, hbox: 8, fragments: 5 });
assertMetrics("app/view/Detail.view.xml", { tags: 275, vbox: 40, hbox: 10, fragments: 10 });
assertMetrics("app/view/Analytics.view.xml", { tags: 50, vbox: 5, hbox: 3, fragments: 4 });
assertMetrics("app/view/fragment/WorkflowAnalyticsTopline.fragment.xml", { tags: 32, vbox: 2, hbox: 3, fragments: 0 });
assertMetrics("app/view/fragment/WorkflowAnalyticsBreakdowns.fragment.xml", { tags: 235, vbox: 6, hbox: 2, fragments: 0 });
assertMetrics("app/view/fragment/DetailControlRail.fragment.xml", { tags: 95, vbox: 3, hbox: 8, fragments: 1 });
assertMetrics("app/view/fragment/SearchLoadStatePanel.fragment.xml", { tags: 18, vbox: 3, hbox: 4, fragments: 0 });
assertMetrics("app/view/fragment/ChecksExpandedDialog.fragment.xml", { tags: 55, vbox: 3, hbox: 1, fragments: 1 });
assertMetrics("app/view/fragment/BarriersExpandedDialog.fragment.xml", { tags: 55, vbox: 3, hbox: 1, fragments: 1 });

console.log("XML architecture check passed.");
