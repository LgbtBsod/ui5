sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/shared/ControllerResourceCleanup",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/SearchFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRouteRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/internal/SearchStartupBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/internal/SearchViewLoadBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/internal/SearchAnalyticsIntentBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchAnalyticsRailRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchRateProgress",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/StatusChipClassRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SemanticDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchToolbarContracts"
], function (
    ControllerResourceCleanup,
    SearchFacade,
    ControllerRouteRuntime,
    ModelStateRuntime,
    SchedulingRuntime,
    SearchStartupBehavior,
    SearchViewLoadBehavior,
    SearchAnalyticsIntentBehavior,
    SearchViewportRuntime,
    SearchAnalyticsRailRuntime,
    SearchRateProgress,
    SearchViewStateRuntime,
    StatusChipClassRuntime,
    SemanticDomRuntime,
    ModelContracts,
    NavigationContracts,
    SearchToolbarContracts
) {
    "use strict";

    var VIEW_MODEL = ModelContracts.MODELS.VIEW;
    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var TOKENS = ModelContracts.TOKENS;
    var PATHS = SearchToolbarContracts.PATHS;

    function getBundleText(oController, sKey) {
        var oI18nModel = oController.getModel && oController.getModel("i18n");
        var oBundle = oI18nModel && oI18nModel.getResourceBundle && oI18nModel.getResourceBundle();
        return oBundle && oBundle.getText ? oBundle.getText(sKey) : "";
    }

    function syncSemanticRegions(oController) {
        SemanticDomRuntime.syncControllerTarget(oController, "searchAnalyticsRailRegion", {
            role: "region",
            "aria-label": getBundleText(oController, "kpiRailAriaLabel")
        });
        SemanticDomRuntime.syncControllerTarget(oController, "searchFilterCard", {
            role: "search",
            "aria-label": getBundleText(oController, "filtersAriaLabel")
        });
        SemanticDomRuntime.syncControllerTarget(oController, "searchResultsShell", {
            role: "region",
            "aria-label": getBundleText(oController, "searchResultsAriaLabel")
        });
        SemanticDomRuntime.syncControllerTarget(oController, "searchResultsSummaryRail", {
            role: "status",
            "aria-live": "polite",
            "aria-label": getBundleText(oController, "searchSummaryRailAriaLabel")
        });
    }

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
        oController._iLocationSuggestRequestVersion = 0;
        oController._searchRateProgress = SearchRateProgress;
        oController._sSearchUiSessionKey = SearchViewStateRuntime.resolveSearchUiSessionKey();
        oController.setModel(SearchViewStateRuntime.createViewModel(oController._sSearchUiSessionKey), VIEW_MODEL);
        initSearchToolbarState(oController);
        ControllerRouteRuntime.attachMatched(oController, [
            { name: NavigationContracts.ROUTES.SEARCH, handler: oController._onSearchMatched },
            { name: NavigationContracts.ROUTES.DETAIL, handler: oController._onDetailSearchContextMatched },
            { name: NavigationContracts.ROUTES.ANALYTICS, handler: oController._onAnalyticsMatched }
        ]);
        SearchStartupBehavior.syncSmartControlAvailability(oController);
        SearchViewportRuntime.bindSearchViewportRuntime(oController);
    }

    function onAfterRendering(oController) {
        var oStateModel = oController.getModel && oController.getModel(STATE_MODEL);
        var oShellModel = oController.getModel && oController.getModel(ModelContracts.MODELS.SHELL);
        var sCurrentRouteName = String(oStateModel && oStateModel.getProperty("/currentRouteName") || "").trim();
        var sLayout = String(oShellModel && oShellModel.getProperty(ModelContracts.MODEL_PATHS.SHELL_LAYOUT) || "").trim();
        syncSemanticRegions(oController);
        StatusChipClassRuntime.syncView(oController);
        if (!oController._bSearchInitialRouteHandled && sCurrentRouteName === NavigationContracts.ROUTES.SEARCH) {
            oController._bSearchInitialRouteHandled = true;
            oController._onSearchMatched();
            return;
        }
        if (sCurrentRouteName === NavigationContracts.ROUTES.DETAIL) {
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
        SearchViewportRuntime.unbindSearchViewportRuntime(oController);
        SearchAnalyticsRailRuntime.clearAnalyticsRefreshTimer(oController);
        oController._iAnalyticsRailPulseTimer = SchedulingRuntime.clearTimer(oController._iAnalyticsRailPulseTimer);
        oController._iSearchWorkingHintTimer = SchedulingRuntime.clearTimer(oController._iSearchWorkingHintTimer);
        oController._iLocationSuggestTimer = SchedulingRuntime.clearTimer(oController._iLocationSuggestTimer);
        oController._iLocationSuggestTimer = null;
        oController._aLocationSuggestCache = [];
        oController._sLocationSuggestNeedle = "";
        oController._iLocationSuggestRequestVersion = 0;
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
        oController._bSearchInitialRouteHandled = true;
        oController._bSearchRouteActive = true;
        SearchStartupBehavior.onSearchMatched(oController);
        fnApplyAnalyticsDrilldownIntent();
    }

    function onDetailSearchContextMatched(oController, oEvent) {
        var oArgs = oEvent && oEvent.getParameter && oEvent.getParameter("arguments");
        var sLayout = String((oArgs && oArgs.layout) || "");
        if (sLayout === NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN) {
            return;
        }
        oController._bSearchRouteActive = false;
        SearchAnalyticsRailRuntime.clearAnalyticsRefreshTimer(oController);
        SearchViewportRuntime.captureSearchScrollPosition(oController);
        SearchStartupBehavior.syncSearchContextForDetailRoute(oController);
    }

    function onAnalyticsMatched(oController) {
        oController._bSearchRouteActive = false;
        SearchAnalyticsRailRuntime.clearAnalyticsRefreshTimer(oController);
        SearchViewportRuntime.captureSearchScrollPosition(oController);
        SearchViewLoadBehavior.resetTransientFeedback(oController);
    }

    return {
        applyAnalyticsDrilldownIntent: SearchAnalyticsIntentBehavior.applyAnalyticsDrilldownIntent,
        onAfterRendering: onAfterRendering,
        onAnalyticsMatched: onAnalyticsMatched,
        onDetailSearchContextMatched: onDetailSearchContextMatched,
        onExit: onExit,
        onInit: onInit,
        onSearchMatched: onSearchMatched
    };
});
