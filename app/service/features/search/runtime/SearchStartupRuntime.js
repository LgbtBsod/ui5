sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ProgressiveReadinessContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ReadinessTelemetryContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DebugLogger"
], function (ControllerViewStateRuntime, ModelStateRuntime, ModelContracts, OperationSourceContracts, ProgressiveReadinessContracts, ReadinessTelemetryContracts, ReadinessTelemetryRuntime, DebugLogger) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;
    var SEARCH_READINESS = ProgressiveReadinessContracts.SEARCH;

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
        return !!ModelStateRuntime.read(oController, STATE_MODEL, SEARCH_READINESS.FLAGS.FORCE_REFRESH_ON_RETURN, false)
            && !!ControllerViewStateRuntime.get(oController, "/hasSearched", false);
    }

    function clearSearchRefreshFlag(oController) {
        ModelStateRuntime.write(oController, STATE_MODEL, SEARCH_READINESS.FLAGS.FORCE_REFRESH_ON_RETURN, false);
    }

    function refreshSearchTableIfNeeded(oController, sSource, mHooks) {
        if (!shouldRefreshSearchOnReturn(oController) || !ControllerViewStateRuntime.get(oController, "/smartTableReady", false)) {
            return;
        }
        clearSearchRefreshFlag(oController);
        mHooks.rebind({
            source: sSource || SEARCH_SOURCES.SEARCH_RETRY
        });
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
        mHooks.clearInitialAnalyticsSchedule();
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
