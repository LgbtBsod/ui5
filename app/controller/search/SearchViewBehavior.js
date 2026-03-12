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
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchStartupRuntime"
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
    SearchStartupRuntime
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
        SearchSelectionRuntime.syncSearchTableRuntimeState(oController, SearchSelectionRuntime.resolveSearchInnerTable(oController));
        ControllerViewStateRuntime.set(oController, "/tableBusy", false);
    }

    function onSearchMatched(oController) {
        SearchStartupRuntime.onSearchMatched(oController, {
            bindSearchAnalytics: function () {
                oController._runSearchAnalytics = function (mInput) {
                    return SearchCommandPolicy.analytics(oController, mInput);
                };
            },
            bindSearchViewportRuntime: function () {
                SearchViewportRuntime.bindSearchViewportRuntime(oController);
            },
            bindSearchWorkingText: function () {
                oController._resolveSearchWorkingText = function () {
                    return ControllerTextRuntime.getText(oController, "workingMessageLong", [], "Working...");
                };
            },
            bootstrap: function (mInput) {
                return SearchCommandPolicy.bootstrap(oController, mInput);
            },
            clearInitialAnalyticsSchedule: function () {
                SearchAnalyticsRailRuntime.clearInitialAnalyticsSchedule(oController);
            },
            rebind: function (mInput) {
                return SearchCommandPolicy.rebind(oController, mInput);
            },
            restoreSearchScrollPosition: function () {
                SearchViewportRuntime.restoreSearchScrollPosition(oController);
            },
            scheduleInitialAnalytics: function (fnAfterStart) {
                return SearchAnalyticsRailRuntime.scheduleInitialAnalytics(oController, fnAfterStart);
            },
            syncSmartControlAvailability: function () {
                syncSmartControlAvailability(oController);
            }
        });
    }

    function syncSearchContextForDetailRoute(oController) {
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

    function onSmartTableInitialise(oController) {
        return SearchSmartTableRuntime.onSmartTableInitialise(oController, {
            bindTableRuntime: function (oInnerTable, fnAfterBind) {
                SearchSelectionRuntime.bindSearchTableRuntime(oController, oInnerTable, fnAfterBind);
            },
            bindViewportRuntime: function () {
                SearchViewportRuntime.bindSearchViewportRuntime(oController);
            },
            configureResultTable: function (oInnerTable, bForce) {
                SearchSelectionRuntime.configureSearchResultTable(oController, oInnerTable, bForce);
            },
            onItemPress: oController.onSearchTableItemPress,
            onSelectionChange: oController.onSearchTableSelectionChange,
            refreshTableIfNeeded: function (sSource) {
                SearchStartupRuntime.refreshSearchTableIfNeeded(oController, sSource, {
                    rebind: function (mInput) {
                        return SearchCommandPolicy.rebind(oController, mInput);
                    }
                });
            },
            resolveInnerTable: function () {
                return SearchSelectionRuntime.resolveSearchInnerTable(oController);
            },
            scheduleViewportSync: function (bImmediate) {
                SearchViewportRuntime.scheduleSearchViewportSync(oController, bImmediate);
            },
            syncRequestWindow: function () {
                SearchViewStateRuntime.syncSearchTableRequestWindow(oController);
            },
            wireRateProgress: function (oInnerTable) {
                SearchRateProgress.wireTable(oController, oInnerTable);
            }
        });
    }

    function onBeforeSmartTableRebind(oController, oEvent) {
        return SearchSmartTableRuntime.onBeforeSmartTableRebind(oController, oEvent, {
            applyRebindPolicy: function (mInput) {
                return SearchCommandPolicy.applyRebindPolicy(oController, mInput);
            },
            beginSearchLoadingFeedback: function () {
                SearchLoadingFeedbackRuntime.beginSearchLoadingFeedback(oController);
            },
            bindPendingSearchLoad: function (oInnerTable) {
                SearchLoadingFeedbackRuntime.bindPendingSearchLoad(oController, oInnerTable, {
                    applyLoadError: function (sErrorMessage) {
                        SearchLoadRuntime.applyLoadError(oController, sErrorMessage);
                    },
                    applyLoadSuccess: function (aRows) {
                        SearchLoadRuntime.applyLoadSuccess(oController, aRows);
                    },
                    readRows: function () {
                        return readSearchRows(oController, oInnerTable);
                    },
                    afterSuccess: function () {
                        SearchSelectionRuntime.syncSearchTableRuntimeState(oController, oInnerTable);
                        SearchViewportRuntime.bindSearchViewportRuntime(oController);
                        SearchViewportRuntime.scheduleSearchViewportSync(oController, true);
                    }
                });
            },
            configureResultTable: function (oInnerTable, bForce) {
                SearchSelectionRuntime.configureSearchResultTable(oController, oInnerTable, bForce);
            },
            resolveInnerTable: function () {
                return SearchSelectionRuntime.resolveSearchInnerTable(oController);
            },
            scheduleViewportSync: function (bImmediate) {
                SearchViewportRuntime.scheduleSearchViewportSync(oController, bImmediate);
            },
            settlePendingSearchLoad: function (oInnerTable, oError) {
                SearchLoadingFeedbackRuntime.settlePendingSearchLoad(oController, {
                    innerTable: oInnerTable,
                    error: oError
                }, {
                    applyLoadError: function (sErrorMessage) {
                        SearchLoadRuntime.applyLoadError(oController, sErrorMessage);
                    },
                    applyLoadSuccess: function (aRows) {
                        SearchLoadRuntime.applyLoadSuccess(oController, aRows);
                    },
                    readRows: function () {
                        return readSearchRows(oController, oInnerTable);
                    },
                    afterSuccess: function () {
                        SearchSelectionRuntime.syncSearchTableRuntimeState(oController, oInnerTable);
                        SearchViewportRuntime.bindSearchViewportRuntime(oController);
                        SearchViewportRuntime.scheduleSearchViewportSync(oController, true);
                    }
                });
            },
            syncRequestWindow: function () {
                SearchViewStateRuntime.syncSearchTableRequestWindow(oController);
            }
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
