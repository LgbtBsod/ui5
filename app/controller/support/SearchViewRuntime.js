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
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchShortcutRuntime"
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
    SearchShortcutRuntime
) {
    "use strict";

    var SEARCH_WORKING_HINT_MS = 2000;
    var SEARCH_INITIAL_ANALYTICS_DELAY_MS = 400;

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

    function clearSearchWorkingHintTimer(oController) {
        oController._iSearchWorkingHintTimer = SchedulingRuntime.clearTimer(oController._iSearchWorkingHintTimer);
    }

    function clearPendingSearchLoad(oController) {
        oController._iPendingSearchLoadTimer = SchedulingRuntime.clearTimer(oController._iPendingSearchLoadTimer);
        oController._oPendingSearchLoad = null;
    }

    function isSearchBindingSettled(oInnerTable) {
        var oBinding = oInnerTable && oInnerTable.getBinding && oInnerTable.getBinding("items");
        if (!oInnerTable || !oBinding) {
            return false;
        }
        if (typeof oInnerTable.getBusy === "function" && oInnerTable.getBusy()) {
            return false;
        }
        if (typeof oBinding.isPending === "function" && oBinding.isPending()) {
            return false;
        }
        if (oBinding.bPendingRequest || oBinding.bPendingRefresh) {
            return false;
        }
        if (typeof oBinding.isLengthFinal === "function") {
            return !!oBinding.isLengthFinal();
        }
        return true;
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

    function settlePendingSearchLoad(oController, oOptions) {
        var oPending = oController._oPendingSearchLoad;
        var oInnerTable = oOptions && oOptions.innerTable;
        var oError = oOptions && oOptions.error;
        var sErrorMessage;
        if (!oPending || oPending.settled) {
            return;
        }
        oPending.settled = true;
        clearPendingSearchLoad(oController);
        hideSearchWorkingHint(oController);
        if (oError) {
            sErrorMessage = String((oError && (oError.message || oError.statusText)) || "Search request failed").trim();
            SearchLoadRuntime.applyLoadError(oController, sErrorMessage);
            return;
        }
        SearchLoadRuntime.applyLoadSuccess(oController, readSearchRows(oController, oInnerTable));
        SearchSelectionRuntime.syncSearchTableRuntimeState(oController, oInnerTable);
        SearchViewportRuntime.bindSearchViewportRuntime(oController);
        SearchViewportRuntime.scheduleSearchViewportSync(oController, true);
    }

    function bindPendingSearchLoad(oController, oInnerTable) {
        var iStartedAt = Date.now();
        var fnPoll;
        clearPendingSearchLoad(oController);
        oController._oPendingSearchLoad = { settled: false };
        if (!oInnerTable || typeof oInnerTable.attachEventOnce !== "function") {
            return;
        }
        oInnerTable.attachEventOnce("updateFinished", function () {
            settlePendingSearchLoad(oController, { innerTable: oInnerTable });
        });
        fnPoll = function () {
            if (!oController._oPendingSearchLoad || oController._oPendingSearchLoad.settled) {
                return;
            }
            if (isSearchBindingSettled(oInnerTable) || (Date.now() - iStartedAt) >= 8000) {
                settlePendingSearchLoad(oController, { innerTable: oInnerTable });
                return;
            }
            oController._iPendingSearchLoadTimer = SchedulingRuntime.restartTimer(
                oController._iPendingSearchLoadTimer,
                fnPoll,
                250
            );
        };
        oController._iPendingSearchLoadTimer = SchedulingRuntime.restartTimer(0, fnPoll, 250);
    }

    function hideSearchWorkingHint(oController) {
        clearSearchWorkingHintTimer(oController);
        ControllerViewStateRuntime.set(oController, "/filterHintVisible", false);
        ControllerViewStateRuntime.set(oController, "/filterHintText", "");
    }

    function isSearchLoading(oController) {
        return !!(
            ControllerViewStateRuntime.get(oController, "/tableBusy", false)
            || ControllerViewStateRuntime.get(oController, "/searchActionBusy", false)
            || ModelStateRuntime.read(oController, "state", "/isLoading", false)
        );
    }

    function scheduleSearchWorkingHint(oController) {
        clearSearchWorkingHintTimer(oController);
        oController._iSearchWorkingHintTimer = SchedulingRuntime.restartTimer(0, function () {
            if (!isSearchLoading(oController)) {
                return;
            }
            ControllerViewStateRuntime.set(oController, "/filterHintVisible", true);
            ControllerViewStateRuntime.set(oController, "/filterHintType", "Information");
            ControllerViewStateRuntime.set(
                oController,
                "/filterHintText",
                ControllerTextRuntime.getText(oController, "workingMessageLong", [], "Working...")
            );
        }, SEARCH_WORKING_HINT_MS);
    }

    function beginSearchLoadingFeedback(oController) {
        scheduleSearchWorkingHint(oController);
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

    function clearAnalyticsRefreshTimer(oController) {
        oController._iAnalyticsRefreshTimer = SchedulingRuntime.clearTimer(oController._iAnalyticsRefreshTimer);
    }

    function clearInitialAnalyticsSchedule(oController) {
        oController._iInitialAnalyticsTimer = SchedulingRuntime.clearTimer(oController._iInitialAnalyticsTimer);
        if (oController._iInitialAnalyticsIdleId && window.cancelIdleCallback) {
            window.cancelIdleCallback(oController._iInitialAnalyticsIdleId);
            oController._iInitialAnalyticsIdleId = null;
        }
    }

    function resolveAnalyticsRefreshMs(oController) {
        var oStateModel = ControllerModelRuntime.state(oController);
        var iMs = Number(TimeConfigService.read(oStateModel, "analyticsRefreshMs"));
        return Number.isFinite(iMs) && iMs >= 1000 ? iMs : 300000;
    }

    function pulseAnalyticsRailUpdate(oController) {
        var oRail = oController.byId("searchAnalyticsRail");
        if (!oRail) {
            return;
        }
        oController._iAnalyticsRailPulseTimer = SchedulingRuntime.clearTimer(oController._iAnalyticsRailPulseTimer);
        ControlStyleRuntime.restart(oRail, "searchAnalyticsRailPulse");
        oController._iAnalyticsRailPulseTimer = SchedulingRuntime.restartTimer(0, function () {
            ControlStyleRuntime.disable(oRail, "searchAnalyticsRailPulse");
            oController._iAnalyticsRailPulseTimer = null;
        }, 520);
    }

    function refreshAnalyticsRail(oController, mOptions) {
        var bSilent = !!(mOptions && mOptions.silent);
        if (!bSilent) {
            ControllerViewStateRuntime.set(oController, "/analyticsRailBusy", true);
            ControllerViewStateRuntime.set(oController, "/analyticsError", "");
        }
        return SearchCommandPolicy.analytics(oController, { intent: "refreshRail", silent: bSilent }).then(function (vResult) {
            if (bSilent) {
                pulseAnalyticsRailUpdate(oController);
            }
            return vResult;
        });
    }

    function scheduleAnalyticsRefresh(oController) {
        clearAnalyticsRefreshTimer(oController);
        oController._iAnalyticsRefreshTimer = SchedulingRuntime.restartTimer(0, function () {
            refreshAnalyticsRail(oController, { silent: true });
            scheduleAnalyticsRefresh(oController);
        }, resolveAnalyticsRefreshMs(oController));
    }

    function bindAnalyticsRefreshTimer(oController) {
        var oStateModel = ControllerModelRuntime.state(oController);
        if (!oStateModel || oController._oAnalyticsRefreshBinding) {
            return;
        }
        if (!oController._fnAnalyticsRefreshChanged) {
            oController._fnAnalyticsRefreshChanged = function () {
                scheduleAnalyticsRefresh(oController);
            };
        }
        oController._oAnalyticsRefreshBinding = oStateModel.bindProperty("/timers/analyticsRefreshMs");
        oController._oAnalyticsRefreshBinding.attachChange(oController._fnAnalyticsRefreshChanged);
    }

    function syncSmartControlAvailability(oController) {
        SearchSelectionRuntime.syncSearchTableRuntimeState(oController, SearchSelectionRuntime.resolveSearchInnerTable(oController));
        ControllerViewStateRuntime.set(oController, "/tableBusy", false);
    }

    function shouldRefreshSearchOnReturn(oController) {
        return !!ModelStateRuntime.read(oController, "state", "/searchForceRefreshOnReturn", false)
            && !!ControllerViewStateRuntime.get(oController, "/hasSearched", false);
    }

    function clearSearchRefreshFlag(oController) {
        ModelStateRuntime.write(oController, "state", "/searchForceRefreshOnReturn", false);
    }

    function refreshSearchTableIfNeeded(oController, sSource) {
        if (!shouldRefreshSearchOnReturn(oController) || !ControllerViewStateRuntime.get(oController, "/smartTableReady", false)) {
            return;
        }
        clearSearchRefreshFlag(oController);
        SearchCommandPolicy.rebind(oController, { source: sSource || "searchReturn" });
    }

    function onSearchMatched(oController) {
        syncSmartControlAvailability(oController);
        SearchViewportRuntime.bindSearchViewportRuntime(oController);
        logStartupMetric(oController, "firstRouteReady");
        ControllerViewStateRuntime.set(oController, "/bootstrapBusy", true);
        ControllerViewStateRuntime.set(oController, "/analyticsBusy", false);
        ControllerViewStateRuntime.set(oController, "/analyticsRailBusy", true);
        ControllerViewStateRuntime.set(oController, "/analyticsError", "");
        clearInitialAnalyticsSchedule(oController);
        Promise.resolve(SearchCommandPolicy.bootstrap(oController, { reason: "routeMatched" }))
            .catch(function () {
                return null;
            })
            .finally(function () {
                var fnStartAnalytics = function () {
                    oController._iInitialAnalyticsIdleId = null;
                    oController._iInitialAnalyticsTimer = null;
                    logStartupMetric(oController, "analyticsStarted");
                    refreshAnalyticsRail(oController, { silent: false });
                    scheduleAnalyticsRefresh(oController);
                };
                if (window.requestIdleCallback) {
                    oController._iInitialAnalyticsIdleId = window.requestIdleCallback(fnStartAnalytics, { timeout: 800 });
                    return;
                }
                oController._iInitialAnalyticsTimer = SchedulingRuntime.restartTimer(0, fnStartAnalytics, SEARCH_INITIAL_ANALYTICS_DELAY_MS);
            });
        SearchViewportRuntime.restoreSearchScrollPosition(oController);
        refreshSearchTableIfNeeded(oController, "routeMatchedReturn");
    }

    function syncSearchContextForDetailRoute(oController) {
        syncSmartControlAvailability(oController);
        SearchViewportRuntime.bindSearchViewportRuntime(oController);
        SearchViewportRuntime.scheduleSearchViewportSync(oController, false);
    }

    function onSmartTableInitialise(oController) {
        var oInnerTable = SearchSelectionRuntime.resolveSearchInnerTable(oController);
        ControllerViewStateRuntime.set(oController, "/smartTableReady", true);
        if (!oInnerTable) {
            return;
        }
        SearchSelectionRuntime.configureSearchResultTable(oController, oInnerTable, true);
        SearchSelectionRuntime.bindSearchTableRuntime(oController, oInnerTable, function () {
            SearchViewportRuntime.bindSearchViewportRuntime(oController);
            SearchViewportRuntime.scheduleSearchViewportSync(oController, false);
        });
        if (oInnerTable.removeSelections) {
            oInnerTable.removeSelections(true);
        }
        if (oInnerTable.attachSelectionChange) {
            oInnerTable.attachSelectionChange(oController.onSearchTableSelectionChange, oController);
        }
        SearchViewStateRuntime.syncSearchTableRequestWindow(oController);
        SearchRateProgress.wireTable(oController, oInnerTable);
        SearchViewportRuntime.bindSearchViewportRuntime(oController);
        SearchViewportRuntime.scheduleSearchViewportSync(oController, true);
        refreshSearchTableIfNeeded(oController, "smartTableInitialise");
    }

    function onBeforeSmartTableRebind(oController, oEvent) {
        var oBindingParams = oEvent && oEvent.getParameter && oEvent.getParameter("bindingParams");
        var oStateModel = ControllerModelRuntime.state(oController);
        var oInnerTable = SearchSelectionRuntime.resolveSearchInnerTable(oController);
        SearchSelectionRuntime.configureSearchResultTable(oController, oInnerTable, true);
        SearchViewStateRuntime.syncSearchTableRequestWindow(oController);
        ControllerViewStateRuntime.set(oController, "/tableBusy", true);
        scheduleSearchWorkingHint(oController);
        SearchViewportRuntime.scheduleSearchViewportSync(oController, false);
        bindPendingSearchLoad(oController, oInnerTable);
        SearchCommandPolicy.applyRebindPolicy(oController, {
            source: "beforeRebind",
            bindingParams: oBindingParams || {},
            state: (oStateModel && oStateModel.getData && oStateModel.getData()) || {},
            onDataReceived: function (oDataEvent) {
                var oError = oDataEvent && oDataEvent.getParameter
                    && (oDataEvent.getParameter("error") || oDataEvent.getParameter("data") && oDataEvent.getParameter("data").error);
                settlePendingSearchLoad(oController, {
                    innerTable: oInnerTable,
                    error: oError
                });
            }
        }).catch(function (oError) {
            settlePendingSearchLoad(oController, {
                innerTable: oInnerTable,
                error: oError
            });
            return Promise.reject(oError);
        });
    }

    function openWorkflowAnalytics(oController) {
        NavigationIntentService.navigateToAnalytics(oController);
        return Promise.resolve();
    }

    function closeWorkflowAnalytics(oController) {
        NavigationIntentService.navigateBackFromAnalytics(oController);
    }

    function runExport(oController, sEntity) {
        var aSelectedRowIds = ControllerViewStateRuntime.get(oController, "/selectedRowIds", []) || [];
        var iBackendTop = Number(ModelStateRuntime.read(oController, "state", "/searchBackendTop", 0)) || 0;
        ControllerViewStateRuntime.set(oController, "/exportBusy", true);
        return SearchCommandPolicy.exportFlow(oController, {
            entity: sEntity || "screen",
            selectedRowIds: Array.isArray(aSelectedRowIds) ? aSelectedRowIds.slice(0) : [],
            backendTop: iBackendTop
        }).finally(function () {
            ControllerViewStateRuntime.set(oController, "/exportBusy", false);
        });
    }

    return {
        bindAnalyticsRefreshTimer: bindAnalyticsRefreshTimer,
        bindPowerUserShortcuts: SearchShortcutRuntime.bindPowerUserShortcuts,
        bindSearchViewportRuntime: SearchViewportRuntime.bindSearchViewportRuntime,
        beginSearchLoadingFeedback: beginSearchLoadingFeedback,
        captureSearchScrollPosition: SearchViewportRuntime.captureSearchScrollPosition,
        clearSelection: SearchSelectionRuntime.clearSelection,
        clearAnalyticsRefreshTimer: clearAnalyticsRefreshTimer,
        closeWorkflowAnalytics: closeWorkflowAnalytics,
        focusSearchResults: SearchSelectionRuntime.focusSearchResults,
        focusSearchToolbar: SearchSelectionRuntime.focusSearchToolbar,
        onBeforeSmartTableRebind: onBeforeSmartTableRebind,
        syncSearchContextForDetailRoute: syncSearchContextForDetailRoute,
        onSearchMatched: onSearchMatched,
        onSmartTableInitialise: onSmartTableInitialise,
        openWorkflowAnalytics: openWorkflowAnalytics,
        runExport: runExport,
        scrollToSearchResultsToolbar: SearchViewportRuntime.scrollToSearchResultsToolbar,
        scrollToSearchFilters: SearchViewportRuntime.scrollToSearchFilters,
        selectVisibleRows: SearchSelectionRuntime.selectVisibleRows,
        setSearchActionBusy: setSearchActionBusy,
        syncSmartControlAvailability: syncSmartControlAvailability,
        unbindPowerUserShortcuts: SearchShortcutRuntime.unbindPowerUserShortcuts,
        unbindSearchViewportRuntime: SearchViewportRuntime.unbindSearchViewportRuntime
    };
});
