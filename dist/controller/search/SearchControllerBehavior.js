sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchActionBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchFormatterBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchLifecycleBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchFilterLifecycleBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchLocationSuggestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchRequestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchToolbarDialogRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchAnalyticsIntentBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchViewBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerActionBusyRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchToolbarContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ReadinessTelemetryContracts",
    "sap/ui/core/Item"
], function (SearchActionBehavior, SearchFormatterBehavior, SearchLifecycleBehavior, SearchFilterLifecycleBehavior, SearchLocationSuggestRuntime, SearchRequestRuntime, SearchToolbarDialogRuntime, SearchAnalyticsIntentBehavior, SearchViewBehavior, ControllerActionBusyRuntime, ControllerViewStateRuntime, ReadinessTelemetryRuntime, SearchToolbarContracts, SearchLoadRuntime, ModelContracts, OperationSourceContracts, ReadinessTelemetryContracts, Item) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;
    var STATE_MODEL = MODELS.STATE;
    var PATHS = SearchToolbarContracts.PATHS;

    function applyAnalyticsDrilldownIntent(oController) {
        return SearchAnalyticsIntentBehavior.applyAnalyticsDrilldownIntent(oController, {
            intentPath: PATHS.ANALYTICS_DRILLDOWN_INTENT,
            smartTableReadyPath: "/smartTableReady",
            source: SEARCH_SOURCES.ANALYTICS_DRILLDOWN,
            stateModel: STATE_MODEL
        });
    }

    function buildFacadeContext(oController) {
        return {
            smartControls: oController.getOwnerComponent()._ctx.smartControls,
            stateModel: oController.getModel(STATE_MODEL),
            viewModel: oController.getModel("view")
        };
    }

    function executeSearchFacade(oController, sMethod, mInput) {
        return oController.executeFacadeMethod(oController._facade, sMethod, mInput || {}, buildFacadeContext(oController));
    }

    function buildSearchCommandPolicy(oController) {
        if (!oController._facade) {
            return null;
        }
        return {
            rebind: function (_oController, mInput) {
                return executeSearchFacade(oController, "rebind", mInput);
            }
        };
    }



    return {
        _withActionBusy: function (sPath, fnAction) {
            return ControllerActionBusyRuntime.withActionBusy(this, sPath, fnAction);
        },

        onInit: function () {
            SearchLifecycleBehavior.onInit(this);
        },

        onAfterRendering: function () {
            SearchLifecycleBehavior.onAfterRendering(this);
        },

        onExit: function () {
            SearchLifecycleBehavior.onExit(this);
        },

        _onSearchMatched: function () {
            SearchLifecycleBehavior.onSearchMatched(this, applyAnalyticsDrilldownIntent.bind(null, this));
        },

        _onDetailSearchContextMatched: function (oEvent) {
            SearchLifecycleBehavior.onDetailSearchContextMatched(this, oEvent);
        },

        _onAnalyticsMatched: function () {
            SearchLifecycleBehavior.onAnalyticsMatched(this);
        },

        onSmartFilterInitialise: function () {
            SearchFilterLifecycleBehavior.onSmartFilterInitialise(this, applyAnalyticsDrilldownIntent.bind(null, this));
        },

        onLocationKeySuggest: function (oEvent) {
            SearchLocationSuggestRuntime.runLocationSuggest(this, oEvent, Item);
        },

        onLocationKeySuggestionSelected: function (oEvent) {
            SearchLocationSuggestRuntime.applyLocationSuggestionSelection(oEvent);
        },

        onSmartFilterChanged: function () {
            SearchFilterLifecycleBehavior.onSmartFilterChanged(this);
        },

        onSmartTableInitialise: function () {
            SearchViewBehavior.onSmartTableInitialise(this);
        },

        onBeforeSmartTableRebind: function (oEvent) {
            SearchRequestRuntime.syncToolbarRequestInputs(this);
            SearchViewBehavior.onBeforeSmartTableRebind(this, oEvent);
        },

        onSmartSearch: function () {
            ReadinessTelemetryRuntime.markControllerStage(this, ReadinessTelemetryContracts.STAGES.SEARCH_INTERACTION_READY, {
                action: "smartSearch"
            });
            SearchViewBehavior.beginSearchLoadingFeedback(this);
            return SearchFilterLifecycleBehavior.onSmartSearch(this, function (sBusyPath, fnAction) {
                return ControllerActionBusyRuntime.withActionBusy(this, sBusyPath, fnAction, function (bBusy) {
                    SearchViewBehavior.setSearchActionBusy(this, bBusy);
                }.bind(this));
            }.bind(this));
        },

        onRetrySearchLoad: function () {
            SearchLoadRuntime.markLoading(this);
            SearchViewBehavior.beginSearchLoadingFeedback(this);
            return this._facade && this._facade.rebind
                ? executeSearchFacade(this, "rebind", {
                    source: SEARCH_SOURCES.SEARCH_RETRY
                }).finally(function () {
                    SearchLoadRuntime.setLoadStatus(this, { isLoading: false, isBusy: false, loadError: false });
                }.bind(this)).catch(function (oError) {
                    SearchLoadRuntime.applyLoadError(this, String((oError && oError.message) || "Unable to load search results."));
                    return Promise.reject(oError);
                }.bind(this))
                : Promise.resolve();
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
            SearchFilterLifecycleBehavior.onMaxRowsChange(this, oEvent);
        },

        onBackendTopChange: function (oEvent) {
            SearchFilterLifecycleBehavior.onBackendTopChange(this, oEvent);
        },

        onSearchModeToggle: function (oEvent) {
            SearchFilterLifecycleBehavior.onSearchModeToggle(this, oEvent);
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
            return SearchToolbarDialogRuntime.applySearchSortSettings(this, {
                sortKey: oSortItem && oSortItem.getKey && oSortItem.getKey(),
                sortDescending: bSortDescending
            }, {
                ControllerViewStateRuntime: ControllerViewStateRuntime,
                SearchCommandPolicy: buildSearchCommandPolicy(this)
            });
        },

        onOpenSearchGroupDialog: function () {
            SearchToolbarDialogRuntime.openGroupDialog(this);
        },

        onSearchGroupDialogConfirm: function (oEvent) {
            var oGroupItem = oEvent && oEvent.getParameter && oEvent.getParameter("groupItem");
            var bGroupDescending = !!(oEvent && oEvent.getParameter && oEvent.getParameter("groupDescending"));
            return SearchToolbarDialogRuntime.applySearchGroupSettings(this, {
                groupKey: oGroupItem && oGroupItem.getKey && oGroupItem.getKey(),
                groupDescending: bGroupDescending
            }, {
                ControllerViewStateRuntime: ControllerViewStateRuntime,
                SearchCommandPolicy: buildSearchCommandPolicy(this)
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
            executeSearchFacade(this, "buildFilter", {
                intent: SEARCH_SOURCES.CHECKS_SEGMENT,
                key: oEvent.getParameter("key")
            });
        },

        onBarriersFailSegmentChange: function (oEvent) {
            executeSearchFacade(this, "buildFilter", {
                intent: SEARCH_SOURCES.BARRIERS_SEGMENT,
                key: oEvent.getParameter("key")
            });
        },

        onExportScreen: function () {
            return SearchActionBehavior.onExportScreen(this);
        },

        onExportMenuAction: function (oEvent) {
            return SearchActionBehavior.onExportMenuAction(this, oEvent);
        }
    };
});
