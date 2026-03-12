sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchRateProgress",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControlStyleRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/util/TimeConfigService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchViewportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchShortcutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchAnalyticsRailRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadingFeedbackRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSmartTableRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchActionRuntime"
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
    SearchActionRuntime
) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var STATE_MODEL = MODELS.STATE;
    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;
    function resolveStartupPerf(oController) {
        var oOwner = oController && oController.getOwnerComponent && oController.getOwnerComponent();
        if (!oOwner) {
            return null;
        }
        oOwner._startupPerf = oOwner._startupPerf || {
            t0: (window.performance && typeof window.performance.now === "function") ? window.performance.now() : Date.now(),
            firstRouteReadyLogged: false,
            analyticsStartedLogged: false
        };
        return oOwner._startupPerf;
    }

    function nowMs() {
        return (window.performance && typeof window.performance.now === "function") ? window.performance.now() : Date.now();
    }

    function logStartupMetric(oController, sEvent) {
        var oPerf = resolveStartupPerf(oController);
        var iDelta;
        if (!oPerf || !oPerf.t0) {
            return;
        }
        if (sEvent === "firstRouteReady" && oPerf.firstRouteReadyLogged) {
            return;
        }
        if (sEvent === "analyticsStarted" && oPerf.analyticsStartedLogged) {
            return;
        }
        iDelta = Math.max(0, Math.round(nowMs() - oPerf.t0));
        if (sEvent === "firstRouteReady") {
            oPerf.firstRouteReadyLogged = true;
            console.info("[Startup] first route ready:", iDelta + "ms");
            return;
        }
        if (sEvent === "analyticsStarted") {
            oPerf.analyticsStartedLogged = true;
            console.info("[Startup] analytics started:", iDelta + "ms");
        }
    }

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

    function shouldRefreshSearchOnReturn(oController) {
        return !!ModelStateRuntime.read(oController, STATE_MODEL, "/searchForceRefreshOnReturn", false)
            && !!ControllerViewStateRuntime.get(oController, "/hasSearched", false);
    }

    function clearSearchRefreshFlag(oController) {
        ModelStateRuntime.write(oController, STATE_MODEL, "/searchForceRefreshOnReturn", false);
    }

    function refreshSearchTableIfNeeded(oController, sSource) {
        if (!shouldRefreshSearchOnReturn(oController) || !ControllerViewStateRuntime.get(oController, "/smartTableReady", false)) {
            return;
        }
        clearSearchRefreshFlag(oController);
        SearchCommandPolicy.rebind(oController, { source: sSource || SEARCH_SOURCES.SEARCH_RETRY });
    }

    function onSearchMatched(oController) {
        syncSmartControlAvailability(oController);
        SearchViewportRuntime.bindSearchViewportRuntime(oController);
        logStartupMetric(oController, "firstRouteReady");
        ControllerViewStateRuntime.set(oController, "/bootstrapBusy", true);
        ControllerViewStateRuntime.set(oController, "/analyticsBusy", false);
        ControllerViewStateRuntime.set(oController, "/analyticsRailBusy", true);
        ControllerViewStateRuntime.set(oController, "/analyticsError", "");
        oController._runSearchAnalytics = function (mInput) {
            return SearchCommandPolicy.analytics(oController, mInput);
        };
        oController._resolveSearchWorkingText = function () {
            return ControllerTextRuntime.getText(oController, "workingMessageLong", [], "Working...");
        };
        SearchAnalyticsRailRuntime.clearInitialAnalyticsSchedule(oController);
        Promise.resolve(SearchCommandPolicy.bootstrap(oController, { reason: "routeMatched" }))
            .catch(function () {
                return null;
            })
            .finally(SearchAnalyticsRailRuntime.scheduleInitialAnalytics(oController, function () {
                logStartupMetric(oController, "analyticsStarted");
            }));
        SearchViewportRuntime.restoreSearchScrollPosition(oController);
        refreshSearchTableIfNeeded(oController, "routeMatchedReturn");
    }

    function syncSearchContextForDetailRoute(oController) {
        syncSmartControlAvailability(oController);
        SearchViewportRuntime.bindSearchViewportRuntime(oController);
        SearchViewportRuntime.scheduleSearchViewportSync(oController, false);
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
                refreshSearchTableIfNeeded(oController, sSource);
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
        clearSelection: SearchSelectionRuntime.clearSelection,
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
        selectVisibleRows: SearchSelectionRuntime.selectVisibleRows,
        setSearchActionBusy: setSearchActionBusy,
        syncSmartControlAvailability: syncSmartControlAvailability,
        unbindPowerUserShortcuts: SearchShortcutRuntime.unbindPowerUserShortcuts,
        unbindSearchViewportRuntime: SearchViewportRuntime.unbindSearchViewportRuntime
    };
});
