sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TimerDefaults"
], function (NavigationContracts, TimerDefaults) {
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
            LOAD_ERROR_MESSAGE: "loadErrorMessage",
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
            SEARCH_LOADED_PATH: NavigationContracts.READINESS_PATHS.SEARCH,
            DETAIL_LOADED_PATH: NavigationContracts.READINESS_PATHS.DETAIL,
            ANALYTICS_LOADED_PATH: NavigationContracts.READINESS_PATHS.ANALYTICS
        }),
        LAYOUTS: NavigationContracts.LAYOUTS,
        STATE_TIMERS: Object.freeze(
            Object.keys(TimerDefaults).reduce(function (acc, sKey) {
                if (TimerDefaults[sKey] && TimerDefaults[sKey].defaultValue !== undefined) {
                    acc[sKey] = TimerDefaults[sKey].defaultValue;
                }
                return acc;
            }, {})
        )
    });
});
