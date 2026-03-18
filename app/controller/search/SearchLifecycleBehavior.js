sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/shared/ControllerResourceCleanup",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/SearchFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRouteRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchViewBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchRateProgress",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchToolbarContracts"
], function (
    ControllerResourceCleanup,
    SearchFacade,
    ControllerRouteRuntime,
    ModelStateRuntime,
    SchedulingRuntime,
    SearchViewBehavior,
    SearchRateProgress,
    SearchViewStateRuntime,
    ModelContracts,
    NavigationContracts,
    SearchToolbarContracts
) {
    "use strict";

    var VIEW_MODEL = ModelContracts.MODELS.VIEW;
    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var TOKENS = ModelContracts.TOKENS;
    var PATHS = SearchToolbarContracts.PATHS;

    function initSearchToolbarState(oController) {
        if (!String(ModelStateRuntime.read(oController, STATE_MODEL, PATHS.SEARCH_SORT_KEY, "")).trim()) {
            ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_SORT_KEY, TOKENS.DATE_CHECK);
        }
        if (typeof ModelStateRuntime.read(oController, STATE_MODEL, PATHS.SEARCH_SORT_DESCENDING, undefined) !== "boolean") {
            ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_SORT_DESCENDING, true);
        }
        if (!String(ModelStateRuntime.read(oController, STATE_MODEL, PATHS.SEARCH_GROUP_KEY, "")).trim()) {
            ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_GROUP_KEY, "");
        }
        if (typeof ModelStateRuntime.read(oController, STATE_MODEL, PATHS.SEARCH_GROUP_DESCENDING, undefined) !== "boolean") {
            ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_GROUP_DESCENDING, false);
        }
    }

    function onInit(oController) {
        oController._facade = new SearchFacade();
        oController._bSearchInitialRouteHandled = false;
        oController._bSearchRouteActive = false;
        oController._iAnalyticsRefreshTimer = null;
        oController._iAnalyticsRailPulseTimer = null;
        oController._iSearchWorkingHintTimer = null;
        oController._iLocationSuggestTimer = null;
        oController._aLocationSuggestCache = [];
        oController._sLocationSuggestNeedle = "";
        oController._searchRateProgress = SearchRateProgress;
        oController._sSearchUiSessionKey = SearchViewStateRuntime.resolveSearchUiSessionKey();
        oController.setModel(SearchViewStateRuntime.createViewModel(oController._sSearchUiSessionKey), VIEW_MODEL);
        initSearchToolbarState(oController);
        ControllerRouteRuntime.attachMatched(oController, [
            { name: NavigationContracts.ROUTES.SEARCH, handler: oController._onSearchMatched },
            { name: NavigationContracts.ROUTES.DETAIL, handler: oController._onDetailSearchContextMatched },
            { name: NavigationContracts.ROUTES.DETAIL_LAYOUT, handler: oController._onDetailSearchContextMatched },
            { name: NavigationContracts.ROUTES.ANALYTICS, handler: oController._onAnalyticsMatched }
        ]);
        SearchViewBehavior.syncSmartControlAvailability(oController);
        SearchViewBehavior.bindPowerUserShortcuts(oController);
        SearchViewBehavior.bindSearchViewportRuntime(oController);
    }

    function onAfterRendering(oController) {
        var oStateModel = oController.getModel && oController.getModel(STATE_MODEL);
        var sCurrentRouteName = String(oStateModel && oStateModel.getProperty("/currentRouteName") || "").trim();
        var sLayout = String(oStateModel && oStateModel.getProperty("/layout") || "").trim();
        if (!oController._bSearchInitialRouteHandled && sCurrentRouteName === NavigationContracts.ROUTES.SEARCH) {
            oController._bSearchInitialRouteHandled = true;
            oController._onSearchMatched();
            return;
        }
        if (sCurrentRouteName === NavigationContracts.ROUTES.DETAIL || sCurrentRouteName === NavigationContracts.ROUTES.DETAIL_LAYOUT) {
            oController._onDetailSearchContextMatched({
                getParameter: function (sName) {
                    if (sName === "arguments") {
                        return { layout: sLayout };
                    }
                    return null;
                }
            });
        }
    }

    function onExit(oController) {
        ControllerRouteRuntime.detachAllMatched(oController);
        SearchViewBehavior.unbindPowerUserShortcuts(oController);
        SearchViewBehavior.unbindSearchViewportRuntime(oController);
        SearchViewBehavior.clearAnalyticsRefreshTimer(oController);
        oController._iAnalyticsRailPulseTimer = SchedulingRuntime.clearTimer(oController._iAnalyticsRailPulseTimer);
        oController._iSearchWorkingHintTimer = SchedulingRuntime.clearTimer(oController._iSearchWorkingHintTimer);
        oController._iLocationSuggestTimer = SchedulingRuntime.clearTimer(oController._iLocationSuggestTimer);
        oController._iLocationSuggestTimer = null;
        oController._aLocationSuggestCache = [];
        oController._sLocationSuggestNeedle = "";
        if (oController._oAnalyticsRefreshBinding) {
            oController._oAnalyticsRefreshBinding = ControllerResourceCleanup.destroyBinding(oController._oAnalyticsRefreshBinding, oController._fnAnalyticsRefreshChanged);
        }
        oController._fnAnalyticsRefreshChanged = null;
        if (oController._oSearchSortDialog) {
            oController._oSearchSortDialog.destroy();
            oController._oSearchSortDialog = null;
        }
        if (oController._oSearchGroupDialog) {
            oController._oSearchGroupDialog.destroy();
            oController._oSearchGroupDialog = null;
        }
        oController._bSearchInitialRouteHandled = null;
        oController._bSearchRouteActive = null;
    }

    function onSearchMatched(oController, fnApplyAnalyticsDrilldownIntent) {
        oController._bSearchRouteActive = true;
        SearchViewBehavior.onSearchMatched(oController);
        fnApplyAnalyticsDrilldownIntent();
    }

    function onDetailSearchContextMatched(oController, oEvent) {
        var oArgs = oEvent && oEvent.getParameter && oEvent.getParameter("arguments");
        var sLayout = String((oArgs && oArgs.layout) || "");
        if (sLayout === NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN) {
            return;
        }
        oController._bSearchRouteActive = false;
        SearchViewBehavior.clearAnalyticsRefreshTimer(oController);
        SearchViewBehavior.captureSearchScrollPosition(oController);
        SearchViewBehavior.syncSearchContextForDetailRoute(oController);
    }

    function onAnalyticsMatched(oController) {
        oController._bSearchRouteActive = false;
        SearchViewBehavior.clearAnalyticsRefreshTimer(oController);
        SearchViewBehavior.captureSearchScrollPosition(oController);
    }

    return {
        onAfterRendering: onAfterRendering,
        onAnalyticsMatched: onAnalyticsMatched,
        onDetailSearchContextMatched: onDetailSearchContextMatched,
        onExit: onExit,
        onInit: onInit,
        onSearchMatched: onSearchMatched
    };
});
