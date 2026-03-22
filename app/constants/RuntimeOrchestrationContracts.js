sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ShellPaneConstants"
], function (NavigationConstants, ShellPaneConstants) {
    "use strict";

    return Object.freeze({
        ROOT_PATH: "/readiness/metrics",
        STARTED_AT_PATH: "/readiness/metrics/startedAtMs",
        LAST_STAGE_PATH: "/readiness/metrics/lastStage",
        STAGES: Object.freeze({
            SHELL_READY: "shellReady",
            SEARCH_ROUTE_READY: "searchRouteReady",
            SEARCH_INTERACTION_READY: "searchInteractionReady",
            DETAIL_READY: "detailReady",
            ANALYTICS_READY: "analyticsReady",
            DEFERRED_DIALOG_READY: "deferredDialogReady"
        }),
        SEARCH: Object.freeze({
            FLAGS: Object.freeze({
                FORCE_REFRESH_ON_RETURN: "/searchForceRefreshOnReturn"
            }),
            LOAD_ERROR_MESSAGE: "Search request failed",
            STARTUP_EVENTS: Object.freeze({
                ANALYTICS_STARTED: "analyticsStarted",
                FIRST_ROUTE_READY: "firstRouteReady"
            }),
            STARTUP_LOG_KEYS: Object.freeze({
                ANALYTICS_STARTED: "analyticsStartedLogged",
                FIRST_ROUTE_READY: "firstRouteReadyLogged"
            })
        }),
        PANES: Object.freeze({
            SEARCH_LOADED_PATH: ShellPaneConstants.READINESS_PATHS.SEARCH,
            DETAIL_LOADED_PATH: ShellPaneConstants.READINESS_PATHS.DETAIL,
            ANALYTICS_LOADED_PATH: ShellPaneConstants.READINESS_PATHS.ANALYTICS
        }),
        LAYOUTS: NavigationConstants.LAYOUTS,
        STATE_TIMERS: Object.freeze({
            heartbeatMs: 270000,
            lockStatusMs: 60000,
            gcdMs: 300000,
            idleMs: 570000,
            autoSaveIntervalMs: 150000,
            autoSaveDebounceMs: 1200,
            networkGraceMs: 60000,
            lockRefreshCooldownMs: 150000,
            analyticsRefreshMs: 900000,
            retryBaseDelayMs: 500,
            retryMaxDelayMs: 10000,
            cacheToleranceMs: 5500
        })
    });
});
