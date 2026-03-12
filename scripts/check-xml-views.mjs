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
const workflowAnalyticsToplinePrimaryCards = read("app/view/fragment/WorkflowAnalyticsToplinePrimaryCards.fragment.xml");
const workflowAnalyticsToplineRateCards = read("app/view/fragment/WorkflowAnalyticsToplineRateCards.fragment.xml");
const workflowAnalyticsMessageStack = read("app/view/fragment/WorkflowAnalyticsMessageStack.fragment.xml");
const workflowStatusMetaChips = read("app/view/fragment/WorkflowStatusMetaChips.fragment.xml");
const workflowAnalyticsBreakdowns = read("app/view/fragment/WorkflowAnalyticsBreakdowns.fragment.xml");
const detailAccessDeniedScene = read("app/view/fragment/DetailAccessDeniedScene.fragment.xml");
const detailInfoCardsHost = read("app/view/fragment/DetailInfoCardsHost.fragment.xml");
const detailChecksBusyShell = read("app/view/fragment/DetailChecksBusyShell.fragment.xml");
const detailBarriersBusyShell = read("app/view/fragment/DetailBarriersBusyShell.fragment.xml");
const detailCreateModeBanner = read("app/view/fragment/DetailCreateModeBanner.fragment.xml");
const detailChecksSectionShell = read("app/view/fragment/DetailChecksSectionShell.fragment.xml");
const detailBarriersSectionShell = read("app/view/fragment/DetailBarriersSectionShell.fragment.xml");
const detailChecksSectionToolbar = read("app/view/fragment/DetailChecksSectionToolbar.fragment.xml");
const detailBarriersSectionToolbar = read("app/view/fragment/DetailBarriersSectionToolbar.fragment.xml");
const detailChecksEmptyState = read("app/view/fragment/DetailChecksEmptyState.fragment.xml");
const detailBarriersEmptyState = read("app/view/fragment/DetailBarriersEmptyState.fragment.xml");
const locationValueHelpDialog = read("app/view/fragment/LocationValueHelpDialog.fragment.xml");
const lockSwitchStatus = read("app/view/fragment/LockSwitchStatus.fragment.xml");
const searchLoadRetryHint = read("app/view/fragment/SearchLoadRetryHint.fragment.xml");

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

if (!/items="\{path:'view>\/infoCards', factory: '\.infoCardFactory'\}"/.test(detailInfoCardsHost)) {
  fail("DetailInfoCardsHost.fragment.xml must use infoCardFactory for infoCards GridList.");
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
assertIncludes("app/view/Analytics.view.xml", /WorkflowAnalyticsToplinePrimaryCards/, "Analytics.view.xml must compose topline KPI cards via primary fragment.");
assertIncludes("app/view/Analytics.view.xml", /WorkflowAnalyticsToplineRateCards/, "Analytics.view.xml must compose topline KPI cards via rate fragment.");
assertIncludes("app/view/Analytics.view.xml", /WorkflowStatusMetaChips/, "Analytics.view.xml must compose topline meta chips via shared fragment.");
assertIncludes("app/view/Analytics.view.xml", /WorkflowAnalyticsMessageStack/, "Analytics.view.xml must compose analytics status messaging via shared fragment.");
assertIncludes("app/view/fragment/WorkflowAnalyticsToplinePrimaryCards.fragment.xml", /kpiCardDanger/, "Analytics topline primary cards must expose semantic failed KPI styling.");
assertIncludes("app/view/fragment/WorkflowAnalyticsToplineRateCards.fragment.xml", /kpiRate/, "Analytics topline rate cards must expose governed KPI rate styling.");
assertIncludes("app/view/fragment/DetailChecksSectionShell.fragment.xml", /DetailChecksSectionToolbar/, "DetailChecksSectionShell.fragment.xml must compose checks toolbar via fragment.");
assertIncludes("app/view/fragment/DetailBarriersSectionShell.fragment.xml", /DetailBarriersSectionToolbar/, "DetailBarriersSectionShell.fragment.xml must compose barriers toolbar via fragment.");
assertIncludes("app/view/Detail.view.xml", /DetailAccessDeniedScene/, "Detail.view.xml must compose denied state via fragment.");
assertIncludes("app/view/Detail.view.xml", /DetailCreateModeBanner/, "Detail.view.xml must compose create-mode banner via fragment.");
assertIncludes("app/view/Detail.view.xml", /DetailInfoCardsHost/, "Detail.view.xml must compose info-card host via fragment.");
assertIncludes("app/view/fragment/DetailChecksSectionShell.fragment.xml", /DetailChecksBusyShell/, "DetailChecksSectionShell.fragment.xml must compose checks busy shell via fragment.");
assertIncludes("app/view/fragment/DetailBarriersSectionShell.fragment.xml", /DetailBarriersBusyShell/, "DetailBarriersSectionShell.fragment.xml must compose barriers busy shell via fragment.");
assertIncludes("app/view/Detail.view.xml", /DetailChecksSectionShell/, "Detail.view.xml must compose checks table section shell via fragment.");
assertIncludes("app/view/Detail.view.xml", /DetailBarriersSectionShell/, "Detail.view.xml must compose barriers table section shell via fragment.");
assertIncludes("app/view/fragment/DetailChecksSectionShell.fragment.xml", /DetailChecksEmptyState/, "DetailChecksSectionShell.fragment.xml must compose checks empty state via fragment.");
assertIncludes("app/view/fragment/DetailBarriersSectionShell.fragment.xml", /DetailBarriersEmptyState/, "DetailBarriersSectionShell.fragment.xml must compose barriers empty state via fragment.");
assertIncludes("app/view/fragment/SearchLoadStatePanel.fragment.xml", /SearchLoadRetryHint/, "SearchLoadStatePanel.fragment.xml must compose retry-hint via fragment.");

if (!/<t:Table[\s\S]*rows="\{selected>\/checks\}"/.test(checksExpandedDialog) || !/<Table[\s\S]*items="\{selected>\/checks\}"/.test(checksExpandedDialog)) {
  fail("ChecksExpandedDialog.fragment.xml must keep dual-table contract.");
}

if (!/<t:Table[\s\S]*rows="\{selected>\/barriers\}"/.test(barriersExpandedDialog) || !/<Table[\s\S]*items="\{selected>\/barriers\}"/.test(barriersExpandedDialog)) {
  fail("BarriersExpandedDialog.fragment.xml must keep dual-table contract.");
}

assertMetrics("app/view/App.view.xml", { tags: 30, vbox: 3, hbox: 2, fragments: 0 });
assertMetrics("app/view/Search.view.xml", { tags: 156, vbox: 28, hbox: 8, fragments: 5 });
assertMetrics("app/view/Detail.view.xml", { tags: 210, vbox: 22, hbox: 6, fragments: 9 });
assertMetrics("app/view/Analytics.view.xml", { tags: 54, vbox: 5, hbox: 5, fragments: 6 });
assertMetrics("app/view/fragment/WorkflowAnalyticsToplinePrimaryCards.fragment.xml", { tags: 24, vbox: 3, hbox: 3, fragments: 0 });
assertMetrics("app/view/fragment/WorkflowAnalyticsToplineRateCards.fragment.xml", { tags: 16, vbox: 2, hbox: 2, fragments: 0 });
assertMetrics("app/view/fragment/WorkflowAnalyticsMessageStack.fragment.xml", { tags: 4, vbox: 0, hbox: 0, fragments: 0 });
assertMetrics("app/view/fragment/WorkflowStatusMetaChips.fragment.xml", { tags: 4, vbox: 0, hbox: 0, fragments: 0 });
assertMetrics("app/view/fragment/WorkflowAnalyticsBreakdowns.fragment.xml", { tags: 235, vbox: 6, hbox: 2, fragments: 0 });
assertMetrics("app/view/fragment/DetailAccessDeniedScene.fragment.xml", { tags: 9, vbox: 0, hbox: 0, fragments: 0 });
assertMetrics("app/view/fragment/DetailInfoCardsHost.fragment.xml", { tags: 9, vbox: 3, hbox: 0, fragments: 1 });
assertMetrics("app/view/fragment/DetailChecksBusyShell.fragment.xml", { tags: 3, vbox: 1, hbox: 0, fragments: 1 });
assertMetrics("app/view/fragment/DetailBarriersBusyShell.fragment.xml", { tags: 3, vbox: 1, hbox: 0, fragments: 1 });
assertMetrics("app/view/fragment/DetailCreateModeBanner.fragment.xml", { tags: 7, vbox: 1, hbox: 1, fragments: 0 });
assertMetrics("app/view/fragment/DetailChecksSectionShell.fragment.xml", { tags: 8, vbox: 3, hbox: 0, fragments: 4 });
assertMetrics("app/view/fragment/DetailBarriersSectionShell.fragment.xml", { tags: 8, vbox: 3, hbox: 0, fragments: 4 });
assertMetrics("app/view/fragment/DetailChecksSectionToolbar.fragment.xml", { tags: 10, vbox: 0, hbox: 0, fragments: 0 });
assertMetrics("app/view/fragment/DetailBarriersSectionToolbar.fragment.xml", { tags: 10, vbox: 0, hbox: 0, fragments: 0 });
assertMetrics("app/view/fragment/DetailChecksEmptyState.fragment.xml", { tags: 6, vbox: 1, hbox: 0, fragments: 0 });
assertMetrics("app/view/fragment/DetailBarriersEmptyState.fragment.xml", { tags: 6, vbox: 1, hbox: 0, fragments: 0 });
assertMetrics("app/view/fragment/DetailControlRail.fragment.xml", { tags: 98, vbox: 3, hbox: 8, fragments: 1 });
assertMetrics("app/view/fragment/SearchLoadStatePanel.fragment.xml", { tags: 18, vbox: 3, hbox: 4, fragments: 1 });
assertMetrics("app/view/fragment/SearchLoadRetryHint.fragment.xml", { tags: 4, vbox: 0, hbox: 1, fragments: 0 });
assertMetrics("app/view/fragment/ChecksExpandedDialog.fragment.xml", { tags: 55, vbox: 3, hbox: 1, fragments: 1 });
assertMetrics("app/view/fragment/BarriersExpandedDialog.fragment.xml", { tags: 55, vbox: 3, hbox: 1, fragments: 1 });

console.log("XML architecture check passed.");
