sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchRateProgress",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControlStyleRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
"PRODUCTION_CONTROL_CHECKLIST/service/framework/TimeConfigService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchShortcutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchAnalyticsRailRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadingFeedbackRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSmartTableRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchActionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchStartupRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchSmartTableBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchStartupBehavior"
], function (
    SearchLoadRuntime,
    SearchRateProgress,
    SearchCommandPolicy,
    ControlStyleRuntime,
    ControllerViewStateRuntime,
    ModelStateRuntime,
    SchedulingRuntime,
    ControllerTextRuntime,
    TimeConfigService,
    NavigationIntentService,
    SearchViewStateRuntime,
    ControllerModelRuntime,
    SearchViewportRuntime,
    SearchSelectionRuntime,
    SearchShortcutRuntime,
    StatePaths,
    ModelContracts,
    OperationSourceContracts,
    SearchAnalyticsRailRuntime,
    SearchLoadingFeedbackRuntime,
    SearchSmartTableRuntime,
    SearchActionRuntime,
    SearchStartupRuntime,
    SearchSmartTableBehavior,
    SearchStartupBehavior
) {
    "use strict";

    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;

    function readSearchRows(oController, oInnerTable) {
        var aRows = [];
        var oCtx = oController._ctx && oController._ctx();
        if (oCtx && oCtx.smartControls && oCtx.smartControls.getVisibleRows) {
            aRows = oCtx.smartControls.getVisibleRows() || [];
        }
        if (!aRows.length && oInnerTable) {
            aRows = oInnerTable.getItems ? (oInnerTable.getItems() || []) : [];
        }
        return aRows;
    }

    function setSearchActionBusy(oController, bBusy) {
        var oSearchButton = SearchSelectionRuntime.resolveSmartSearchButton(oController);
        if (!oSearchButton) {
            return;
        }
        ControlStyleRuntime.enable(oSearchButton, "searchGoActionBtn");
        if (typeof oSearchButton.setBusy === "function") {
            oSearchButton.setBusy(!!bBusy);
            oSearchButton.setBusyIndicatorDelay(0);
        }
        if (typeof oSearchButton.setEnabled === "function") {
            oSearchButton.setEnabled(!bBusy);
        }
    }

    function syncSmartControlAvailability(oController) {
        return SearchStartupBehavior.syncSmartControlAvailability(oController);
    }

    function onSearchMatched(oController) {
        return SearchStartupBehavior.onSearchMatched(oController);
    }

    function syncSearchContextForDetailRoute(oController) {
        return SearchStartupBehavior.syncSearchContextForDetailRoute(oController);
    }

    function onSmartTableInitialise(oController) {
        return SearchSmartTableBehavior.onSmartTableInitialise(oController, function (oInnerTable) {
            return readSearchRows(oController, oInnerTable);
        });
    }

    function onBeforeSmartTableRebind(oController, oEvent) {
        return SearchSmartTableBehavior.onBeforeSmartTableRebind(oController, oEvent, function (oInnerTable) {
            return readSearchRows(oController, oInnerTable);
        });
    }

    return {
        bindAnalyticsRefreshTimer: SearchAnalyticsRailRuntime.bindAnalyticsRefreshTimer,
        bindPowerUserShortcuts: SearchShortcutRuntime.bindPowerUserShortcuts,
        bindSearchViewportRuntime: SearchViewportRuntime.bindSearchViewportRuntime,
        beginSearchLoadingFeedback: SearchLoadingFeedbackRuntime.beginSearchLoadingFeedback,
        captureSearchScrollPosition: SearchViewportRuntime.captureSearchScrollPosition,
        clearSelection: function (oController) {
            return SearchSelectionRuntime.clearSelection(oController, function (mInput) {
                return SearchCommandPolicy.selectionChanged(oController, mInput);
            });
        },
        clearAnalyticsRefreshTimer: SearchAnalyticsRailRuntime.clearAnalyticsRefreshTimer,
        closeWorkflowAnalytics: function (oController) {
            return SearchActionRuntime.closeWorkflowAnalytics(oController, {
                navigateBackFromAnalytics: function () {
                    NavigationIntentService.navigateBackFromAnalytics(oController);
                }
            });
        },
        focusSearchResults: SearchSelectionRuntime.focusSearchResults,
        focusSearchToolbar: SearchSelectionRuntime.focusSearchToolbar,
        onBeforeSmartTableRebind: onBeforeSmartTableRebind,
        syncSearchContextForDetailRoute: syncSearchContextForDetailRoute,
        onSearchMatched: onSearchMatched,
        onSmartTableInitialise: onSmartTableInitialise,
        openWorkflowAnalytics: function (oController) {
            return SearchActionRuntime.openWorkflowAnalytics(oController, {
                navigateToAnalytics: function () {
                    NavigationIntentService.navigateToAnalytics(oController);
                }
            });
        },
        runExport: function (oController, sEntity) {
            return SearchActionRuntime.runExport(oController, sEntity, {
                exportFlow: function (mInput) {
                    return SearchCommandPolicy.exportFlow(oController, mInput);
                }
            });
        },
        scrollToSearchResultsToolbar: SearchViewportRuntime.scrollToSearchResultsToolbar,
        scrollToSearchFilters: SearchViewportRuntime.scrollToSearchFilters,
        selectVisibleRows: function (oController) {
            return SearchSelectionRuntime.selectVisibleRows(oController, function (mInput) {
                return SearchCommandPolicy.selectionChanged(oController, mInput);
            });
        },
        setSearchActionBusy: setSearchActionBusy,
        syncSmartControlAvailability: syncSmartControlAvailability,
        unbindPowerUserShortcuts: SearchShortcutRuntime.unbindPowerUserShortcuts,
        unbindSearchViewportRuntime: SearchViewportRuntime.unbindSearchViewportRuntime
    };
});
