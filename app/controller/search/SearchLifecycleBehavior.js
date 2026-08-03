sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/shared/ControllerResourceCleanup",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchActionBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/SearchFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/internal/SearchAnalyticsIntentBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchAnalyticsRailRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchRateProgress",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchStartupRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/StatusChipClassRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SemanticDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchToolbarContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiControlIds"
], function (
    ControllerResourceCleanup,
    SearchActionBehavior,
    SearchFacade,
    ControllerViewStateRuntime,
    ModelStateRuntime,
    SchedulingRuntime,
    SearchAnalyticsIntentBehavior,
    ControllerTextRuntime,
    SearchSelectionRuntime,
    SearchViewportRuntime,
    SearchAnalyticsRailRuntime,
    SearchRateProgress,
    SearchStartupRuntime,
    SearchViewStateRuntime,
    StatusChipClassRuntime,
    SemanticDomRuntime,
    ModelContracts,
    NavigationContracts,
    SearchToolbarContracts,
    UiControlIds
) {
    "use strict";

    var VIEW_MODEL = ModelContracts.MODELS.VIEW;
    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var TOKENS = ModelContracts.TOKENS;
    var PATHS = SearchToolbarContracts.PATHS;

    function runSearchCommand(oController, sMethod, mInput) {
        return SearchActionBehavior.runSearchCommand(oController, sMethod, mInput || {});
    }

    function attachRouteHandlers(oController, aRoutes) {
        if (!Array.isArray(aRoutes) || typeof oController.attachRouteMatched !== "function") {
            return;
        }
        aRoutes.forEach(function (oRouteConfig) {
            var sName = String(oRouteConfig && oRouteConfig.name || "").trim();
            var fnHandler = oRouteConfig && oRouteConfig.handler;
            if (!sName || typeof fnHandler !== "function") {
                return;
            }
            oController.attachRouteMatched(sName, fnHandler);
        });
    }

    function detachRouteHandlers(oController) {
        if (typeof oController.detachAllRouteMatched === "function") {
            oController.detachAllRouteMatched();
        }
    }

    function syncSmartControlAvailability(oController) {
        SearchSelectionRuntime.syncSearchTableRuntimeState(oController, SearchSelectionRuntime.resolveSearchInnerTable(oController));
        ControllerViewStateRuntime.set(oController, "/tableBusy", false);
    }

    function syncSemanticRegions(oController) {
        SemanticDomRuntime.syncControllerTarget(oController, "searchAnalyticsRailRegion", {
            role: "region",
            "aria-label": ControllerTextRuntime.getText(oController, "kpiRailAriaLabel", [], "")
        });
        SemanticDomRuntime.syncControllerTarget(oController, UiControlIds.SEARCH.FILTER_CARD, {
            role: "search",
            "aria-label": ControllerTextRuntime.getText(oController, "filtersAriaLabel", [], "")
        });
        SemanticDomRuntime.syncControllerTarget(oController, UiControlIds.SEARCH.RESULTS_SHELL, {
            role: "region",
            "aria-label": ControllerTextRuntime.getText(oController, "searchResultsAriaLabel", [], "")
        });
        SemanticDomRuntime.syncControllerTarget(oController, "searchResultsSummaryRail", {
            role: "status",
            "aria-live": "polite",
            "aria-label": ControllerTextRuntime.getText(oController, "searchSummaryRailAriaLabel", [], "")
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
        attachRouteHandlers(oController, [
            { name: NavigationContracts.ROUTES.SEARCH, handler: oController._onSearchMatched },
            { name: NavigationContracts.ROUTES.DETAIL, handler: oController._onDetailSearchContextMatched },
            { name: NavigationContracts.ROUTES.ANALYTICS, handler: oController._onAnalyticsMatched }
        ]);
        syncSmartControlAvailability(oController);
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
        detachRouteHandlers(oController);
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
        SearchStartupRuntime.onSearchMatched(oController, {
            bindSearchViewportRuntime: function () {
                SearchViewportRuntime.bindSearchViewportRuntime(oController);
            },
            bindSearchWorkingText: function () {
                oController._resolveSearchWorkingText = function () {
                    return ControllerTextRuntime.getText(oController, "workingMessageLong", []);
                };
            },
            bootstrap: function (mInput) {
                return runSearchCommand(oController, "bootstrap", mInput);
            },
            refreshAnalyticsRail: function () {
                return SearchAnalyticsRailRuntime.refreshAnalyticsRail(oController, {
                    runAnalytics: function (mInput) {
                        return runSearchCommand(oController, "analytics", mInput);
                    }
                });
            },
            rebind: function (mInput) {
                return runSearchCommand(oController, "rebind", mInput);
            },
            rebindTableDirect: function () {
                var oSmartTable = oController && oController.byId && oController.byId(UiControlIds.SEARCH.SMART_TABLE);
                if (!oSmartTable || typeof oSmartTable.rebindTable !== "function") {
                    return false;
                }
                oSmartTable.rebindTable();
                return true;
            },
            restoreSearchScrollPosition: function () {
                SearchViewportRuntime.restoreSearchScrollPosition(oController);
            },
            syncSmartControlAvailability: function () {
                syncSmartControlAvailability(oController);
            }
        });
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
        SearchStartupRuntime.syncSearchContextForDetailRoute(oController, {
            bindSearchViewportRuntime: function () {
                SearchViewportRuntime.bindSearchViewportRuntime(oController);
            },
            scheduleSearchViewportSync: function (bImmediate) {
                SearchViewportRuntime.scheduleSearchViewportSync(oController, bImmediate);
            },
            syncSmartControlAvailability: function () {
                syncSmartControlAvailability(oController);
            }
        });
    }

    function onAnalyticsMatched(oController) {
        oController._bSearchRouteActive = false;
        SearchAnalyticsRailRuntime.clearAnalyticsRefreshTimer(oController);
        SearchViewportRuntime.captureSearchScrollPosition(oController);
        oController._iPendingSearchLoadTimer = SchedulingRuntime.clearTimer(oController._iPendingSearchLoadTimer);
        oController._oPendingSearchLoad = null;
        oController._iSearchWorkingHintTimer = SchedulingRuntime.clearTimer(oController._iSearchWorkingHintTimer);
        ControllerViewStateRuntime.set(oController, "/filterHintVisible", false);
        ControllerViewStateRuntime.set(oController, "/filterHintText", "");
        ControllerViewStateRuntime.set(oController, "/filterHintType", "Information");
        ModelStateRuntime.setMany(oController, STATE_MODEL, {
            "/loadError": false,
            "/loadErrorMessage": ""
        });
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
