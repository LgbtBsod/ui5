sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchActionBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchFilterSegmentBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchFormatterBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchLifecycleBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchFilterLifecycleBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchLocationSuggestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchRequestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchToolbarDialogRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchToolbarBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchInteractionBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchAnalyticsIntentBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchViewBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchToolbarContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/OperationSourceContracts",
    "sap/ui/core/Item"
], function (SearchActionBehavior, SearchFilterSegmentBehavior, SearchFormatterBehavior, SearchLifecycleBehavior, SearchFilterLifecycleBehavior, SearchLocationSuggestRuntime, SearchRequestRuntime, SearchToolbarDialogRuntime, SearchToolbarBehavior, SearchInteractionBehavior, SearchAnalyticsIntentBehavior, SearchViewBehavior, ControllerViewStateRuntime, SearchToolbarContracts, ModelContracts, OperationSourceContracts, Item) {
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

    function withActionBusy(oController, sPath, fnAction) {
        var vResult;
        ControllerViewStateRuntime.setFlag(oController, sPath, true);
        try {
            vResult = fnAction();
        } catch (oError) {
            ControllerViewStateRuntime.setFlag(oController, sPath, false);
            throw oError;
        }
        return Promise.resolve(vResult).finally(function () {
            ControllerViewStateRuntime.setFlag(oController, sPath, false);
        });
    }

    return {
        _withActionBusy: function (sPath, fnAction) {
            return withActionBusy(this, sPath, fnAction);
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
            return SearchInteractionBehavior.onSmartSearch(this);
        },

        onRetrySearchLoad: function () {
            return SearchInteractionBehavior.onRetrySearchLoad(this);
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

