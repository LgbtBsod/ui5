import fs from "fs";
import path from "path";

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

function assertExcludes(filePath, pattern, message) {
  const text = read(filePath);
  if (pattern.test(text)) {
    fail(`${filePath}: ${message}`);
  }
}

const VIEWS_ROOT = path.join("app", "views");
const FRAGMENTS_ROOT = path.join(VIEWS_ROOT, "fragment");
const appViewPath = path.join(VIEWS_ROOT, "App.view.xml");
const searchViewPath = path.join(VIEWS_ROOT, "Search.view.xml");
const detailViewPath = path.join(VIEWS_ROOT, "Detail.view.xml");
const analyticsViewPath = path.join(VIEWS_ROOT, "Analytics.view.xml");

const appView = read(appViewPath);
const searchView = read(searchViewPath);
const detailView = read(detailViewPath);
const analyticsView = read(analyticsViewPath);

if (!/class="chkApp"/.test(appView)) {
  fail("App.view.xml must declare class=\"chkApp\" on the root view.");
}

if (!/searchWorkbenchDock/.test(searchView) || !/SearchActionRail/.test(searchView)) {
  fail("Search.view.xml must keep composed workbench dock with SearchActionRail fragment.");
}

if (!/DetailChecksTables/.test(detailView) || !/DetailBarriersTables/.test(detailView)) {
  fail("Detail.view.xml must compose checks and barriers tables via dedicated fragments.");
}

if (!/DetailControlRail/.test(detailView) || !/DetailAccessDeniedScene/.test(detailView)) {
  fail("Detail.view.xml must compose control rail and access denied scene via fragments.");
}

if (!/showNavButton="true"/.test(analyticsView) || !/analyticsRefreshButton/.test(analyticsView)) {
  fail("Analytics.view.xml must keep route navigation and refresh action.");
}

assertIncludes(path.join(FRAGMENTS_ROOT, "SearchActionRail.fragment.xml"), /searchResultsActionRail/, "SearchActionRail.fragment.xml must keep workbench action rail shell.");
assertIncludes(path.join(FRAGMENTS_ROOT, "SearchActionRail.fragment.xml"), /onCreate/, "SearchActionRail.fragment.xml must expose create action.");
assertIncludes(path.join(FRAGMENTS_ROOT, "SearchActionRail.fragment.xml"), /onBackendTopChange/, "SearchActionRail.fragment.xml must expose backend top request controls.");
assertIncludes(path.join(FRAGMENTS_ROOT, "SearchActionRail.fragment.xml"), /onExportMenuAction/, "SearchActionRail.fragment.xml must expose export menu action.");
assertExcludes(path.join(FRAGMENTS_ROOT, "SearchActionRail.fragment.xml"), /<VBox\b|<HBox\b/, "SearchActionRail.fragment.xml must keep flat toolbar composition.");
assertIncludes(path.join(FRAGMENTS_ROOT, "DetailControlRail.fragment.xml"), /DetailHeroStats/, "DetailControlRail.fragment.xml must compose stats via fragment.");
assertIncludes(path.join(FRAGMENTS_ROOT, "DetailControlRail.fragment.xml"), /DetailSectionAnchorRail/, "DetailControlRail.fragment.xml must compose anchor rail via fragment.");
assertIncludes(path.join(FRAGMENTS_ROOT, "DetailControlRail.fragment.xml"), /DetailControlStatusRow/, "DetailControlRail.fragment.xml must compose status row via fragment.");
assertIncludes(path.join(FRAGMENTS_ROOT, "DetailControlRail.fragment.xml"), /DetailControlActionRow/, "DetailControlRail.fragment.xml must compose action row via fragment.");
assertIncludes(path.join(FRAGMENTS_ROOT, "DetailChecksTables.fragment.xml"), /rows="\{selected>\/checks\}"/, "DetailChecksTables.fragment.xml must keep desktop checks table.");
assertIncludes(path.join(FRAGMENTS_ROOT, "DetailChecksTables.fragment.xml"), /items="\{selected>\/checks\}"/, "DetailChecksTables.fragment.xml must keep phone checks table.");
assertIncludes(path.join(FRAGMENTS_ROOT, "DetailBarriersTables.fragment.xml"), /rows="\{selected>\/barriers\}"/, "DetailBarriersTables.fragment.xml must keep desktop barriers table.");
assertIncludes(path.join(FRAGMENTS_ROOT, "DetailBarriersTables.fragment.xml"), /items="\{selected>\/barriers\}"/, "DetailBarriersTables.fragment.xml must keep phone barriers table.");
assertIncludes(path.join(FRAGMENTS_ROOT, "LocationValueHelpDialog.fragment.xml"), /workflowBusyOverlayShellDialog/, "Location value help must keep workflow busy overlay shell.");
assertIncludes(path.join(FRAGMENTS_ROOT, "ChecksExpandedDialog.fragment.xml"), /workflowBusyOverlayShellDialog/, "Checks expanded dialog must keep workflow busy overlay shell.");
assertIncludes(path.join(FRAGMENTS_ROOT, "BarriersExpandedDialog.fragment.xml"), /workflowBusyOverlayShellDialog/, "Barriers expanded dialog must keep workflow busy overlay shell.");
assertIncludes(path.join(FRAGMENTS_ROOT, "LockSwitchStatus.fragment.xml"), /state>\/lockOperationPending/, "Lock switch must reflect lockOperationPending state.");
assertIncludes(path.join(FRAGMENTS_ROOT, "SearchLoadStatePanel.fragment.xml"), /SearchLoadRetryHint/, "SearchLoadStatePanel.fragment.xml must compose retry-hint via fragment.");

assertMetrics(appViewPath, { tags: 30, vbox: 5, hbox: 2, fragments: 0 });
assertMetrics(searchViewPath, { tags: 20, vbox: 2, hbox: 0, fragments: 6 });
assertMetrics(detailViewPath, { tags: 140, vbox: 22, hbox: 6, fragments: 11 });
assertMetrics(analyticsViewPath, { tags: 60, vbox: 6, hbox: 6, fragments: 8 });
assertMetrics(path.join(FRAGMENTS_ROOT, "SearchActionRail.fragment.xml"), { tags: 70, vbox: 0, hbox: 0, fragments: 0 });
assertMetrics(path.join(FRAGMENTS_ROOT, "DetailControlRail.fragment.xml"), { tags: 12, vbox: 2, hbox: 0, fragments: 4 });
assertMetrics(path.join(FRAGMENTS_ROOT, "DetailHeroStats.fragment.xml"), { tags: 24, vbox: 0, hbox: 6, fragments: 0 });
assertMetrics(path.join(FRAGMENTS_ROOT, "DetailControlStatusRow.fragment.xml"), { tags: 20, vbox: 0, hbox: 0, fragments: 0 });
assertMetrics(path.join(FRAGMENTS_ROOT, "DetailControlActionRow.fragment.xml"), { tags: 44, vbox: 0, hbox: 0, fragments: 1 });
assertMetrics(path.join(FRAGMENTS_ROOT, "DetailChecksTables.fragment.xml"), { tags: 60, vbox: 0, hbox: 0, fragments: 0 });
assertMetrics(path.join(FRAGMENTS_ROOT, "DetailBarriersTables.fragment.xml"), { tags: 60, vbox: 0, hbox: 0, fragments: 0 });

console.log("XML architecture check passed.");
