sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/OperationSourceConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ProgressiveReadinessConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ReadinessTelemetryConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DebugLogger",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchReturnRediscoveryRuntime"
], function (ControllerViewStateRuntime, SchedulingRuntime, ModelContracts, OperationSourceContracts, ProgressiveReadinessContracts, ReadinessTelemetryContracts, ReadinessTelemetryRuntime, DebugLogger, SearchReturnRediscoveryRuntime) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;
    var SEARCH_READINESS = ProgressiveReadinessContracts.SEARCH;
    var SEARCH_RETURN_REFRESH_RETRY_DELAY_MS = 500;
    var SEARCH_RETURN_REFRESH_RETRY_LIMIT = 12;

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
        if (sEvent === SEARCH_READINESS.STARTUP_EVENTS.FIRST_ROUTE_READY && oPerf[SEARCH_READINESS.STARTUP_LOG_KEYS.FIRST_ROUTE_READY]) {
            return;
        }
        if (sEvent === SEARCH_READINESS.STARTUP_EVENTS.ANALYTICS_STARTED && oPerf[SEARCH_READINESS.STARTUP_LOG_KEYS.ANALYTICS_STARTED]) {
            return;
        }
        iDelta = Math.max(0, Math.round(nowMs() - oPerf.t0));
        if (sEvent === SEARCH_READINESS.STARTUP_EVENTS.FIRST_ROUTE_READY) {
            oPerf[SEARCH_READINESS.STARTUP_LOG_KEYS.FIRST_ROUTE_READY] = true;
            DebugLogger.info("SearchStartupRuntime", "first_route_ready", { deltaMs: iDelta });
            return;
        }
        if (sEvent === SEARCH_READINESS.STARTUP_EVENTS.ANALYTICS_STARTED) {
            oPerf[SEARCH_READINESS.STARTUP_LOG_KEYS.ANALYTICS_STARTED] = true;
            DebugLogger.info("SearchStartupRuntime", "analytics_started", { deltaMs: iDelta });
        }
    }

    function shouldRefreshSearchOnReturn(oController) {
        return !!SearchReturnRediscoveryRuntime.readContext(oController)
            || SearchReturnRediscoveryRuntime.hasLegacyRefreshFlag(oController);
    }

    function clearSearchRefreshFlag(oController) {
        SearchReturnRediscoveryRuntime.clearLegacyRefreshFlag(oController);
    }

    function resetReturnRefreshRetry(oController) {
        oController._iSearchReturnRefreshTimer = SchedulingRuntime.clearTimer(oController._iSearchReturnRefreshTimer);
        oController._iSearchReturnRefreshAttempts = 0;
    }

    function scheduleReturnRefreshRetry(oController, sSource, mHooks) {
        if (!shouldRefreshSearchOnReturn(oController)) {
            resetReturnRefreshRetry(oController);
            return;
        }
        oController._iSearchReturnRefreshAttempts = Number(oController._iSearchReturnRefreshAttempts || 0) + 1;
        if (oController._iSearchReturnRefreshAttempts > SEARCH_RETURN_REFRESH_RETRY_LIMIT) {
            resetReturnRefreshRetry(oController);
            return;
        }
        oController._iSearchReturnRefreshTimer = SchedulingRuntime.restartTimer(
            oController._iSearchReturnRefreshTimer,
            function () {
                refreshSearchTableIfNeeded(oController, sSource, mHooks);
            },
            SEARCH_RETURN_REFRESH_RETRY_DELAY_MS
        );
    }

    function refreshSearchTableIfNeeded(oController, sSource, mHooks) {
        var bRebindStarted = false;
        var bCanRebind = false;
        if (!shouldRefreshSearchOnReturn(oController)) {
            resetReturnRefreshRetry(oController);
            return;
        }
        if (!SearchReturnRediscoveryRuntime.readContext(oController)) {
            clearSearchRefreshFlag(oController);
        }
        if (!ControllerViewStateRuntime.get(oController, "/hasSearched", false)) {
            ControllerViewStateRuntime.set(oController, "/hasSearched", true);
        }
        if (mHooks && typeof mHooks.rebindTableDirect === "function") {
            bRebindStarted = !!mHooks.rebindTableDirect();
        }
        bCanRebind = bRebindStarted || !!(mHooks && typeof mHooks.rebind === "function");
        if (!bRebindStarted && mHooks && typeof mHooks.rebind === "function") {
            mHooks.rebind({
                source: sSource || SEARCH_SOURCES.SEARCH_RETRY
            });
        }
        if (!bCanRebind) {
            resetReturnRefreshRetry(oController);
            return;
        }
        scheduleReturnRefreshRetry(oController, sSource || SEARCH_SOURCES.SEARCH_RETRY, mHooks);
    }

    function onSearchMatched(oController, mHooks) {
        mHooks.syncSmartControlAvailability();
        mHooks.bindSearchViewportRuntime();
        logStartupMetric(oController, SEARCH_READINESS.STARTUP_EVENTS.FIRST_ROUTE_READY);
        ReadinessTelemetryRuntime.markControllerStage(oController, ReadinessTelemetryContracts.STAGES.SEARCH_ROUTE_READY, {
            reason: "routeMatched"
        });
        ControllerViewStateRuntime.set(oController, "/bootstrapBusy", true);
        ControllerViewStateRuntime.set(oController, "/analyticsBusy", false);
        ControllerViewStateRuntime.set(oController, "/analyticsRailBusy", false);
        ControllerViewStateRuntime.set(oController, "/analyticsError", "");
        mHooks.bindSearchWorkingText();
        Promise.resolve(mHooks.bootstrap({ reason: "routeMatched" }))
            .catch(function () {
                return null;
            });
        mHooks.restoreSearchScrollPosition();
        refreshSearchTableIfNeeded(oController, "routeMatchedReturn", {
            rebind: mHooks.rebind
        });
    }

    function syncSearchContextForDetailRoute(_oController, mHooks) {
        mHooks.syncSmartControlAvailability();
        mHooks.bindSearchViewportRuntime();
        mHooks.scheduleSearchViewportSync(false);
    }

    return {
        onSearchMatched: onSearchMatched,
        refreshSearchTableIfNeeded: refreshSearchTableIfNeeded,
        syncSearchContextForDetailRoute: syncSearchContextForDetailRoute
    };
});
