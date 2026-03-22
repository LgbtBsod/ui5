sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchActionBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchLifecycleBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchLocationSuggestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchToolbarDialogRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchSmartTableBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchToolbarContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiSemanticConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime",
    "sap/ui/core/Item"
], function (SearchActionBehavior, SearchCommandPolicy, SearchLifecycleBehavior, SearchLocationSuggestRuntime, SearchToolbarDialogRuntime, SearchSmartTableBehavior, ControllerViewStateRuntime, SearchToolbarContracts, SearchViewportRuntime, ModelContracts, OperationSourceContracts, SearchContracts, UiSemanticConstants, SearchViewStateRuntime, Item) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;
    var STATE_MODEL = MODELS.STATE;
    var PATHS = SearchToolbarContracts.PATHS;
    var SEARCH_MODE = SearchContracts.SEARCH_MODE;

    function resolveBundleText(oController, sKey) {
        var oBundle = oController && oController.getResourceBundle && oController.getResourceBundle();
        if (!sKey || !oBundle || !oBundle.getText) {
            return "";
        }
        return String(oBundle.getText(sKey) || "");
    }

    function formatSearchModeChipText(oController, sMode) {
        var sNorm = String(sMode || "").toUpperCase() === SEARCH_MODE.LOOSE ? SEARCH_MODE.LOOSE : SEARCH_MODE.EXACT;
        var sLabel = resolveBundleText(oController, SearchContracts.SEARCH_MODE_LABEL);
        var sModeText = sNorm === SEARCH_MODE.LOOSE
            ? resolveBundleText(oController, SearchContracts.SEARCH_MODE_LOOSE)
            : resolveBundleText(oController, SearchContracts.SEARCH_MODE_EXACT);
        return sLabel + ": " + sModeText;
    }

    function formatWorkflowStageText(oController, sStage) {
        return SearchViewStateRuntime.formatWorkflowStageText(
            oController && oController.getResourceBundle && oController.getResourceBundle(),
            sStage
        );
    }

    function formatWorkflowStageState(sStage) {
        return SearchViewStateRuntime.formatWorkflowStageState(sStage);
    }

    function formatSearchResultsCompactText(oController, iResultCount, bHasRows) {
        var iSafeCount = Math.max(0, Number(iResultCount || 0));
        var sResultsLabel = resolveBundleText(oController, SearchContracts.RESULTS_LABEL);
        if (!bHasRows || !iSafeCount) {
            return sResultsLabel;
        }
        return sResultsLabel + ": " + iSafeCount;
    }

    function formatSearchSelectionSummary(oController, iSelectionCount, sSelectedRowDisplayId) {
        var iSafeCount = Math.max(0, Number(iSelectionCount || 0));
        var sPrimaryId = String(sSelectedRowDisplayId || "").trim();
        if (!iSafeCount) {
            return resolveBundleText(oController, SearchContracts.SEARCH_SELECTION_NONE);
        }
        if (iSafeCount === 1 && sPrimaryId) {
            return resolveBundleText(oController, SearchContracts.SEARCH_SELECTION_PRIMARY_PREFIX) + ": " + sPrimaryId;
        }
        return iSafeCount + " " + resolveBundleText(oController, SearchContracts.SEARCH_SELECTION_UNITS);
    }

    function applyAnalyticsDrilldownIntent(oController) {
        return SearchLifecycleBehavior.applyAnalyticsDrilldownIntent(oController, {
            intentPath: PATHS.ANALYTICS_DRILLDOWN_INTENT,
            smartTableReadyPath: "/smartTableReady",
            source: SEARCH_SOURCES.ANALYTICS_DRILLDOWN,
            stateModel: STATE_MODEL
        });
    }

    return {
        _withActionBusy: function (sPath, fnAction) {
            return SearchActionBehavior.withActionBusy(this, sPath, fnAction);
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
            SearchSmartTableBehavior.onSmartFilterInitialise(this, applyAnalyticsDrilldownIntent.bind(null, this));
        },

        onLocationKeySuggest: function (oEvent) {
            SearchLocationSuggestRuntime.runLocationSuggest(this, oEvent, Item);
        },

        onLocationKeySuggestionSelected: function (oEvent) {
            SearchLocationSuggestRuntime.applyLocationSuggestionSelection(oEvent);
        },

        onSmartFilterChanged: function () {
            SearchSmartTableBehavior.onSmartFilterChanged(this);
        },

        onSmartFilterClear: function () {
            SearchSmartTableBehavior.onSmartFilterClear(this);
        },

        onSmartTableInitialise: function () {
            SearchSmartTableBehavior.onSmartTableInitialise(this, this._readSearchRows.bind(this));
        },

        onBeforeSmartTableRebind: function (oEvent) {
            SearchSmartTableBehavior.onBeforeSmartTableRebind(this, oEvent, this._readSearchRows.bind(this));
        },

        onSmartSearch: function () {
            return SearchActionBehavior.onSmartSearch(this);
        },

        onRetrySearchLoad: function () {
            if (!this._facade || !this._facade.rebind) {
                return Promise.resolve();
            }
            return SearchActionBehavior.onRetrySearchLoad(this);
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
            return SearchViewportRuntime.scrollToSearchFilters(this);
        },

        onScrollSearchResultsToolbarAnchor: function () {
            return SearchViewportRuntime.scrollToSearchResultsToolbar(this);
        },

        onMaxRowsChange: function (oEvent) {
            SearchSmartTableBehavior.onMaxRowsChange(this, oEvent);
        },

        onBackendTopChange: function (oEvent) {
            SearchSmartTableBehavior.onBackendTopChange(this, oEvent);
        },

        onSearchModeToggle: function (oEvent) {
            SearchSmartTableBehavior.onSearchModeToggle(this, oEvent);
        },

        formatSearchModeChipText: function (sMode) {
            return formatSearchModeChipText(this, sMode);
        },

        formatSearchResultsCompactText: function (iResultCount, bHasRows) {
            return formatSearchResultsCompactText(this, iResultCount, bHasRows);
        },

        formatSearchSelectionSummary: function (iSelectionCount, sSelectedRowDisplayId) {
            return formatSearchSelectionSummary(this, iSelectionCount, sSelectedRowDisplayId);
        },
        formatSelectionSummaryState: function () {
            return UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION;
        },
        formatLoadErrorType: function () {
            return UiSemanticConstants.MESSAGE_TYPE.ERROR;
        },
        formatSearchModeState: function () {
            return UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION;
        },
        formatToolbarSelectionState: function (iSelectionCount) {
            return Number(iSelectionCount || 0) > 0
                ? UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS
                : UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION;
        },

        onOpenSearchSortDialog: function () {
            SearchToolbarDialogRuntime.openSortDialog(this);
        },

        onSearchSortDialogConfirm: function (oEvent) {
            return SearchToolbarDialogRuntime.applySearchSortSettings(this, SearchToolbarDialogRuntime.buildSortSettingsFromEvent(oEvent), {
                ControllerViewStateRuntime: ControllerViewStateRuntime,
                SearchCommandPolicy: SearchCommandPolicy
            });
        },

        onOpenSearchGroupDialog: function () {
            SearchToolbarDialogRuntime.openGroupDialog(this);
        },

        onSearchGroupDialogConfirm: function (oEvent) {
            return SearchToolbarDialogRuntime.applySearchGroupSettings(this, SearchToolbarDialogRuntime.buildGroupSettingsFromEvent(oEvent), {
                ControllerViewStateRuntime: ControllerViewStateRuntime,
                SearchCommandPolicy: SearchCommandPolicy
            });
        },

        onOpenWorkflowAnalytics: function (oEvent) {
            return SearchActionBehavior.onOpenWorkflowAnalytics(this, oEvent);
        },

        formatWorkflowStageText: function (sStage) {
            return formatWorkflowStageText(this, sStage);
        },

        formatWorkflowStageState: function (sStage) {
            return formatWorkflowStageState(sStage);
        },

        onSearchTableSelectionChange: function (oEvent) {
            SearchActionBehavior.onTableSelectionChange(this, oEvent);
        },

        onSearchTableItemPress: function (oEvent) {
            return SearchActionBehavior.onTableItemPress(this, oEvent);
        },

        onChecksFailSegmentChange: function (oEvent) {
            SearchCommandPolicy.buildFilter(this, {
                intent: SEARCH_SOURCES.CHECKS_SEGMENT,
                key: oEvent.getParameter("key")
            });
        },

        onBarriersFailSegmentChange: function (oEvent) {
            SearchCommandPolicy.buildFilter(this, {
                intent: SEARCH_SOURCES.BARRIERS_SEGMENT,
                key: oEvent.getParameter("key")
            });
        },

        onExportScreen: function () {
            return SearchActionBehavior.onExportScreen(this);
        },

        onExportMenuAction: function (oEvent) {
            return SearchActionBehavior.onExportMenuAction(this, oEvent);
        },

        _readSearchRows: function (oInnerTable) {
            var aRows = [];
            var oCtx = this._ctx && this._ctx();
            if (oCtx && oCtx.smartControls && oCtx.smartControls.getVisibleRows) {
                aRows = oCtx.smartControls.getVisibleRows() || [];
            }
            if (!aRows.length && oInnerTable) {
                aRows = oInnerTable.getItems ? (oInnerTable.getItems() || []) : [];
            }
            return aRows;
        }
    };
});
