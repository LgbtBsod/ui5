sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/shared/ControllerResourceCleanup",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/SearchFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRouteRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchActionBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchFilterSegmentBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchFormatterBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchLocationSuggestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchRequestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchToolbarDialogRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchRateProgress",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchViewBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UiDecisionCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchToolbarContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/SearchRuntimeContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ProgressiveReadinessContracts",
    "sap/ui/core/Item",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/NavigationContracts"
], function (ControllerResourceCleanup, SearchFacade, ControllerRouteRuntime, ControllerViewStateRuntime, ModelStateRuntime, SearchActionBehavior, SearchFilterSegmentBehavior, SearchFormatterBehavior, SearchCommandPolicy, SearchLocationSuggestRuntime, SearchRequestRuntime, SearchToolbarDialogRuntime, SearchSelectionRuntime, SearchLoadRuntime, SearchRateProgress, SearchViewBehavior, SchedulingRuntime, UiDecisionCoordinator, NavigationIntentService, CreateSentinel, SearchToolbarContracts, SearchViewStateRuntime, ModelContracts, OperationSourceContracts, SearchRuntimeContracts, ProgressiveReadinessContracts, Item, NavigationContracts) {
    "use strict";

    var SEARCH_MODE = SearchRuntimeContracts.SEARCH_MODE;
    var MODELS = ModelContracts.MODELS;
    var TOKENS = ModelContracts.TOKENS;
    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;
    var SEARCH_READINESS = ProgressiveReadinessContracts.SEARCH;
    var STATE_MODEL = MODELS.STATE;
    var VIEW_MODEL = MODELS.VIEW;
    var PATHS = SearchToolbarContracts.PATHS;

    function readAnalyticsDrilldownIntent(oController) {
        return ModelStateRuntime.read(oController, STATE_MODEL, PATHS.ANALYTICS_DRILLDOWN_INTENT, null);
    }

    function clearAnalyticsDrilldownIntent(oController) {
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.ANALYTICS_DRILLDOWN_INTENT, null);
    }

    function applyAnalyticsDrilldownIntent(oController) {
        var oIntent = readAnalyticsDrilldownIntent(oController) || {};
        var sFilterKey = String(oIntent.filterKey || "").trim();
        var sFilterValue = String(oIntent.filterValue || "").trim();
        var oSmartFilterBar = oController.byId("searchSmartFilterBar");
        var oControl;
        var mFilterData;

        if (!sFilterKey || !sFilterValue || !oSmartFilterBar) {
            return false;
        }
        if (typeof oSmartFilterBar.isInitialised === "function" && !oSmartFilterBar.isInitialised()) {
            return false;
        }
        oControl = typeof oSmartFilterBar.getControlByKey === "function" ? oSmartFilterBar.getControlByKey(sFilterKey) : null;
        if (oControl && typeof oControl.setSelectedKey === "function") {
            oControl.setSelectedKey(sFilterValue);
        }
        if (oControl && typeof oControl.setValue === "function") {
            oControl.setValue(sFilterValue);
        }
        if (oControl && typeof oControl.setTokens === "function") {
            oControl.setTokens([]);
        }
        if (typeof oSmartFilterBar.getFilterData === "function" && typeof oSmartFilterBar.setFilterData === "function") {
            mFilterData = Object.assign({}, oSmartFilterBar.getFilterData() || {});
            mFilterData[sFilterKey] = sFilterValue;
            oSmartFilterBar.setFilterData(mFilterData, true);
        }
        clearAnalyticsDrilldownIntent(oController);
        SearchCommandPolicy.buildFilter(oController, { source: SEARCH_SOURCES.ANALYTICS_DRILLDOWN });
        if (ControllerViewStateRuntime.get(oController, "/smartTableReady")) {
            SearchCommandPolicy.rebind(oController, { source: SEARCH_SOURCES.ANALYTICS_DRILLDOWN });
        }
        return true;
    }

    return {
        onInit: function () {
            this._facade = new SearchFacade();
            this._iAnalyticsRefreshTimer = null;
            this._iAnalyticsRailPulseTimer = null;
            this._iSearchWorkingHintTimer = null;
            this._iInitialAnalyticsTimer = null;
            this._iInitialAnalyticsIdleId = null;
            this._iLocationSuggestTimer = null;
            this._aLocationSuggestCache = [];
            this._sLocationSuggestNeedle = "";
            this._searchRateProgress = SearchRateProgress;
            this._sSearchUiSessionKey = SearchViewStateRuntime.resolveSearchUiSessionKey();
            this.setModel(SearchViewStateRuntime.createViewModel(this._sSearchUiSessionKey), VIEW_MODEL);
            if (!String(ModelStateRuntime.read(this, STATE_MODEL, PATHS.SEARCH_SORT_KEY, "")).trim()) {
                ModelStateRuntime.write(this, STATE_MODEL, PATHS.SEARCH_SORT_KEY, TOKENS.DATE_CHECK);
            }
            if (typeof ModelStateRuntime.read(this, STATE_MODEL, PATHS.SEARCH_SORT_DESCENDING, undefined) !== "boolean") {
                ModelStateRuntime.write(this, STATE_MODEL, PATHS.SEARCH_SORT_DESCENDING, true);
            }
            if (!String(ModelStateRuntime.read(this, STATE_MODEL, PATHS.SEARCH_GROUP_KEY, "")).trim()) {
                ModelStateRuntime.write(this, STATE_MODEL, PATHS.SEARCH_GROUP_KEY, "");
            }
            if (typeof ModelStateRuntime.read(this, STATE_MODEL, PATHS.SEARCH_GROUP_DESCENDING, undefined) !== "boolean") {
                ModelStateRuntime.write(this, STATE_MODEL, PATHS.SEARCH_GROUP_DESCENDING, false);
            }
            ControllerRouteRuntime.attachMatched(this, [
                { name: NavigationContracts.ROUTES.SEARCH, handler: this._onSearchMatched },
                { name: NavigationContracts.ROUTES.DETAIL, handler: this._onDetailSearchContextMatched },
                { name: NavigationContracts.ROUTES.DETAIL_LAYOUT, handler: this._onDetailSearchContextMatched }
            ]);
            SearchViewBehavior.bindAnalyticsRefreshTimer(this);
            SearchViewBehavior.syncSmartControlAvailability(this);
            SearchViewBehavior.bindPowerUserShortcuts(this);
            SearchViewBehavior.bindSearchViewportRuntime(this);
        },

        onExit: function () {
            ControllerRouteRuntime.detachAllMatched(this);
            SearchViewBehavior.unbindPowerUserShortcuts(this);
            SearchViewBehavior.unbindSearchViewportRuntime(this);
            SearchViewBehavior.clearAnalyticsRefreshTimer(this);
            this._iAnalyticsRailPulseTimer = SchedulingRuntime.clearTimer(this._iAnalyticsRailPulseTimer);
            this._iSearchWorkingHintTimer = SchedulingRuntime.clearTimer(this._iSearchWorkingHintTimer);
            this._iInitialAnalyticsTimer = SchedulingRuntime.clearTimer(this._iInitialAnalyticsTimer);
            this._iLocationSuggestTimer = SchedulingRuntime.clearTimer(this._iLocationSuggestTimer);
            if (this._iInitialAnalyticsIdleId && window.cancelIdleCallback) {
                window.cancelIdleCallback(this._iInitialAnalyticsIdleId);
                this._iInitialAnalyticsIdleId = null;
            }
            this._iLocationSuggestTimer = null;
            this._aLocationSuggestCache = [];
            this._sLocationSuggestNeedle = "";
            if (this._oAnalyticsRefreshBinding) {
                this._oAnalyticsRefreshBinding = ControllerResourceCleanup.destroyBinding(this._oAnalyticsRefreshBinding, this._fnAnalyticsRefreshChanged);
            }
            this._fnAnalyticsRefreshChanged = null;
            if (this._oSearchSortDialog) {
                this._oSearchSortDialog.destroy();
                this._oSearchSortDialog = null;
            }
            if (this._oSearchGroupDialog) {
                this._oSearchGroupDialog.destroy();
                this._oSearchGroupDialog = null;
            }
        },

        _withActionBusy: function (sViewBusyPath, fnAction, fnSyncControlBusy) {
            if (typeof fnSyncControlBusy === "function") {
                fnSyncControlBusy(true);
            }
            return ControllerViewStateRuntime.withFlag(this, sViewBusyPath, function () {
                if (typeof fnAction === "function") {
                    return fnAction();
                }
                return undefined;
            }).finally(function () {
                if (typeof fnSyncControlBusy === "function") {
                    fnSyncControlBusy(false);
                }
            });
        },

        _syncSmartControlAvailability: function () {
            SearchViewBehavior.syncSmartControlAvailability(this);
        },

        _tryInitialSmartRebind: function () {
            return false;
        },

        _onSearchMatched: function () {
            SearchViewBehavior.onSearchMatched(this);
            applyAnalyticsDrilldownIntent(this);
        },

        _onDetailSearchContextMatched: function (oEvent) {
            var oArgs = oEvent && oEvent.getParameter && oEvent.getParameter("arguments");
            var sLayout = String((oArgs && oArgs.layout) || "");
            if (sLayout === NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN) {
                return;
            }
            SearchViewBehavior.syncSearchContextForDetailRoute(this);
        },

        onSmartFilterInitialise: function () {
            ControllerViewStateRuntime.set(this, "/smartFilterReady", true);
            SearchLocationSuggestRuntime.bindLocationSuggest(this);
            SearchCommandPolicy.buildFilter(this, { source: SEARCH_SOURCES.SMART_FILTER_INIT });
            applyAnalyticsDrilldownIntent(this);
        },

        onLocationKeySuggest: function (oEvent) {
            SearchLocationSuggestRuntime.runLocationSuggest(this, oEvent, Item);
        },

        onLocationKeySuggestionSelected: function (oEvent) {
            SearchLocationSuggestRuntime.applyLocationSuggestionSelection(oEvent);
        },

        onSmartFilterChanged: function () {
            var oSmartFilterBar = this.byId("searchSmartFilterBar");
            if (!oSmartFilterBar || (typeof oSmartFilterBar.isInitialised === "function" && !oSmartFilterBar.isInitialised())) {
                return;
            }
            SearchLocationSuggestRuntime.bindLocationSuggest(this);
            SearchCommandPolicy.buildFilter(this, { source: SEARCH_SOURCES.SMART_FILTER_CHANGED });
        },

        onSmartTableInitialise: function () {
            SearchViewBehavior.onSmartTableInitialise(this);
        },

        onBeforeSmartTableRebind: function (oEvent) {
            SearchRequestRuntime.syncToolbarRequestInputs(this);
            SearchViewBehavior.onBeforeSmartTableRebind(this, oEvent);
        },

        onSmartSearch: function () {
            if (!SearchViewStateRuntime.isSmartControlsReady(this)) {
                return Promise.resolve();
            }
            SearchRequestRuntime.syncToolbarRequestInputs(this);
            SearchLoadRuntime.markLoading(this);
            SearchViewBehavior.beginSearchLoadingFeedback(this);
            return this._withActionBusy("/searchActionBusy", function () {
                return SearchCommandPolicy.executeSearch(this, { source: SEARCH_SOURCES.SMART_SEARCH });
            }.bind(this), function (bBusy) {
                SearchViewBehavior.setSearchActionBusy(this, bBusy);
            }.bind(this));
        },

        onRetrySearchLoad: function () {
            SearchLoadRuntime.markLoading(this);
            SearchViewBehavior.beginSearchLoadingFeedback(this);
            return SearchCommandPolicy.rebind(this, { source: SEARCH_SOURCES.SEARCH_RETRY }).finally(function () {
            SearchLoadRuntime.setLoadStatus(this, { isLoading: false, isBusy: false, loadError: false });
        }.bind(this)).catch(function (oError) {
                SearchLoadRuntime.applyLoadError(this, String((oError && oError.message) || SEARCH_READINESS.LOAD_ERROR_MESSAGE));
                return Promise.reject(oError);
            });
        },

        onCreate: function () {
            return SearchActionBehavior.onCreate(this);
        },

        onCopy: function () {
            return SearchActionBehavior.onCopy(this);
        },

        onSelectVisibleRows: function () {
            return SearchActionBehavior.onSelectVisibleRows(this);
        },

        onClearSelection: function () {
            return SearchActionBehavior.onClearSelection(this);
        },

        onScrollSearchAnchor: function () {
            return SearchViewBehavior.scrollToSearchFilters(this);
        },

        onScrollSearchResultsToolbarAnchor: function () {
            return SearchViewBehavior.scrollToSearchResultsToolbar(this);
        },

        onMaxRowsChange: function (oEvent) {
            SearchRequestRuntime.applyMaxRowsChange(this, oEvent);
        },

        onBackendTopChange: function (oEvent) {
            if (SearchRequestRuntime.applyBackendTopChange(this, oEvent) &&
                ControllerViewStateRuntime.get(this, "/hasSearched") &&
                ControllerViewStateRuntime.get(this, "/smartTableReady")) {
                SearchCommandPolicy.rebind(this, { source: SEARCH_SOURCES.BACKEND_TOP_CHANGE });
            }
        },

        onSearchModeToggle: function (oEvent) {
            var bLoose = !!(oEvent && oEvent.getParameter && oEvent.getParameter("state"));
            ModelStateRuntime.write(this, STATE_MODEL, PATHS.SEARCH_MODE, bLoose ? SEARCH_MODE.LOOSE : SEARCH_MODE.EXACT);
            SearchCommandPolicy.executeSearch(this, { intent: SEARCH_SOURCES.SEARCH_MODE_TOGGLE, state: bLoose });
        },

        formatSearchModeChipText: function (sMode) {
            return SearchFormatterBehavior.formatSearchModeChipText(this, sMode);
        },

        formatSearchResultsCompactText: function (iResultCount, bHasRows) {
            return SearchFormatterBehavior.formatSearchResultsCompactText(this, iResultCount, bHasRows);
        },

        formatSearchSelectionSummary: function (iSelectionCount, sSelectedRowDisplayId) {
            return SearchFormatterBehavior.formatSearchSelectionSummary(this, iSelectionCount, sSelectedRowDisplayId);
        },

        onOpenSearchSortDialog: function () {
            SearchToolbarDialogRuntime.openSortDialog(this);
        },

        onSearchSortDialogConfirm: function (oEvent) {
            var oSortItem = oEvent && oEvent.getParameter && oEvent.getParameter("sortItem");
            var bSortDescending = !!(oEvent && oEvent.getParameter && oEvent.getParameter("sortDescending"));
            SearchToolbarDialogRuntime.applySearchSortSettings(this, {
                sortKey: oSortItem && oSortItem.getKey && oSortItem.getKey(),
                sortDescending: bSortDescending
            }, {
                ControllerViewStateRuntime: ControllerViewStateRuntime,
                SearchCommandPolicy: SearchCommandPolicy
            });
        },

        onOpenSearchGroupDialog: function () {
            SearchToolbarDialogRuntime.openGroupDialog(this);
        },

        onSearchGroupDialogConfirm: function (oEvent) {
            var oGroupItem = oEvent && oEvent.getParameter && oEvent.getParameter("groupItem");
            var bGroupDescending = !!(oEvent && oEvent.getParameter && oEvent.getParameter("groupDescending"));
            SearchToolbarDialogRuntime.applySearchGroupSettings(this, {
                groupKey: oGroupItem && oGroupItem.getKey && oGroupItem.getKey(),
                groupDescending: bGroupDescending
            }, {
                ControllerViewStateRuntime: ControllerViewStateRuntime,
                SearchCommandPolicy: SearchCommandPolicy
            });
        },

        onOpenWorkflowAnalytics: function (oEvent) {
            return SearchViewBehavior.openWorkflowAnalytics(this);
        },

        formatWorkflowStageText: function (sStage) {
            return SearchFormatterBehavior.formatWorkflowStageText(this, sStage);
        },

        formatWorkflowStageState: function (sStage) {
            return SearchFormatterBehavior.formatWorkflowStageState(sStage);
        },

        onSearchTableSelectionChange: function (oEvent) {
            SearchActionBehavior.onTableSelectionChange(this, oEvent);
        },

        onSearchTableItemPress: function (oEvent) {
            return SearchActionBehavior.onTableItemPress(this, oEvent);
        },

        onChecksFailSegmentChange: function (oEvent) {
            SearchFilterSegmentBehavior.onChecksFailSegmentChange(this, oEvent);
        },

        onBarriersFailSegmentChange: function (oEvent) {
            SearchFilterSegmentBehavior.onBarriersFailSegmentChange(this, oEvent);
        },

        _legacySmartTableContractHint: function () {
            return "SearchFilterBuilder.buildFailSegmentFilter";
        },

        onExportScreen: function () {
            return SearchActionBehavior.onExportScreen(this);
        },

        onExportMenuAction: function (oEvent) {
            return SearchActionBehavior.onExportMenuAction(this, oEvent);
        }
    };
});

