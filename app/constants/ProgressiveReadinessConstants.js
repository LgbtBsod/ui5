sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ShellPaneConstants"
], function (NavigationConstants, ShellPaneConstants) {
    "use strict";

    return Object.freeze({
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
        LAYOUTS: NavigationConstants.LAYOUTS
    });
});
