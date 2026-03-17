sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRouteRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsBuilderRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsRefreshRuntime"
], function (ControllerRouteRuntime, ControllerViewStateRuntime, NavigationContracts, ModelContracts, AnalyticsBuilderRuntime, AnalyticsRefreshRuntime) {
    "use strict";

    function clearRefreshTimer(oController) {
        if (oController._iAnalyticsRouteRefreshTimer) {
            clearInterval(oController._iAnalyticsRouteRefreshTimer);
            oController._iAnalyticsRouteRefreshTimer = null;
        }
    }

    function startRefreshTimer(oController) {
        var oStateModel = oController.getModel(ModelContracts.MODELS.STATE);
        var iIntervalMs = Number(oStateModel && oStateModel.getProperty("/timers/analyticsRefreshMs")) || 900000;
        clearRefreshTimer(oController);
        oController._iAnalyticsRouteRefreshTimer = setInterval(function () {
            if (typeof oController._loadAnalytics === "function") {
                oController._loadAnalytics("routeTimer");
            }
        }, iIntervalMs);
    }

    function onInit(oController, oFacade, sRefreshTaskKey) {
        oController._facade = oFacade;
        oController._bAnalyticsInitialRouteHandled = false;
        oController._bAnalyticsRouteActive = false;
        ControllerViewStateRuntime.initModel(oController, function () {
            return AnalyticsBuilderRuntime.createInitialViewState(sRefreshTaskKey);
        });
        AnalyticsBuilderRuntime.applyBuilderSelection(oController);
        ControllerRouteRuntime.attachMatched(oController, [
            { name: NavigationContracts.ROUTES.ANALYTICS, handler: oController._onAnalyticsMatched },
            { name: NavigationContracts.ROUTES.SEARCH, handler: oController._onAnalyticsRouteLeave },
            { name: NavigationContracts.ROUTES.DETAIL, handler: oController._onAnalyticsRouteLeave },
            { name: NavigationContracts.ROUTES.DETAIL_LAYOUT, handler: oController._onAnalyticsRouteLeave }
        ]);
    }

    function onAfterRendering(oController) {
        var oStateModel = oController.getModel && oController.getModel(ModelContracts.MODELS.STATE);
        var sCurrentRouteName = String(oStateModel && oStateModel.getProperty && oStateModel.getProperty("/currentRouteName") || "").trim();
        if (!oController._bAnalyticsInitialRouteHandled && sCurrentRouteName === NavigationContracts.ROUTES.ANALYTICS) {
            oController._onAnalyticsMatched();
        }
        if (sCurrentRouteName !== NavigationContracts.ROUTES.ANALYTICS) {
            clearRefreshTimer(oController);
        }
    }

    function onExit(oController) {
        clearRefreshTimer(oController);
        ControllerRouteRuntime.detachAllMatched(oController);
        if (oController._oAnalyticsYearPicker && typeof oController._oAnalyticsYearPicker.destroy === "function") {
            oController._oAnalyticsYearPicker.destroy();
        }
        if (oController._oAnalyticsReportDialog && typeof oController._oAnalyticsReportDialog.destroy === "function") {
            oController._oAnalyticsReportDialog.destroy();
        }
        oController._oAnalyticsYearPicker = null;
        oController._pAnalyticsYearPicker = null;
        oController._oAnalyticsReportDialog = null;
        oController._pAnalyticsReportDialog = null;
        oController._facade = null;
        oController._bAnalyticsInitialRouteHandled = null;
        oController._bAnalyticsRouteActive = null;
        oController._iAnalyticsRouteRefreshTimer = null;
    }

    function onRouteEnter(oController) {
        oController._bAnalyticsRouteActive = true;
    }

    function onRouteLeave(oController) {
        oController._bAnalyticsRouteActive = false;
        AnalyticsRefreshRuntime.invalidatePolls(oController);
        clearRefreshTimer(oController);
    }

    return {
        onAfterRendering: onAfterRendering,
        onExit: onExit,
        onInit: onInit,
        clearRefreshTimer: clearRefreshTimer,
        onRouteEnter: onRouteEnter,
        onRouteLeave: onRouteLeave,
        startRefreshTimer: startRefreshTimer
    };
});
