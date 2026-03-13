sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchActionBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchFilterSegmentBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchFormatterBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchLifecycleBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchFilterLifecycleBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchLocationSuggestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchRequestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchToolbarDialogRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchToolbarBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchAnalyticsIntentBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchViewBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchToolbarContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ProgressiveReadinessContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ReadinessTelemetryContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "sap/ui/core/Item"
], function (ControllerViewStateRuntime, SearchActionBehavior, SearchFilterSegmentBehavior, SearchFormatterBehavior, SearchLifecycleBehavior, SearchFilterLifecycleBehavior, SearchCommandPolicy, SearchLocationSuggestRuntime, SearchRequestRuntime, SearchToolbarDialogRuntime, SearchToolbarBehavior, SearchAnalyticsIntentBehavior, SearchLoadRuntime, SearchViewBehavior, SearchToolbarContracts, ModelContracts, OperationSourceContracts, ProgressiveReadinessContracts, ReadinessTelemetryContracts, ReadinessTelemetryRuntime, Item) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;
    var SEARCH_READINESS = ProgressiveReadinessContracts.SEARCH;
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

    return {
        onInit: function () {
            SearchLifecycleBehavior.onInit(this);
        },

        onExit: function () {
            SearchLifecycleBehavior.onExit(this);
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

        _onSearchMatched: function () {
            SearchLifecycleBehavior.onSearchMatched(this, applyAnalyticsDrilldownIntent.bind(null, this));
        },

        _onDetailSearchContextMatched: function (oEvent) {
            SearchLifecycleBehavior.onDetailSearchContextMatched(this, oEvent);
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
                return this._withActionBusy(sBusyPath, fnAction, function (bBusy) {
                    SearchViewBehavior.setSearchActionBusy(this, bBusy);
                }.bind(this));
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
            SearchToolbarBehavior.onSearchSortDialogConfirm(this, oEvent);
        },

        onOpenSearchGroupDialog: function () {
            SearchToolbarDialogRuntime.openGroupDialog(this);
        },

        onSearchGroupDialogConfirm: function (oEvent) {
            SearchToolbarBehavior.onSearchGroupDialogConfirm(this, oEvent);
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

        onExportScreen: function () {
            return SearchActionBehavior.onExportScreen(this);
        },

        onExportMenuAction: function (oEvent) {
            return SearchActionBehavior.onExportMenuAction(this, oEvent);
        }
    };
});

