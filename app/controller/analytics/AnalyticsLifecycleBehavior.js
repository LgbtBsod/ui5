sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRouteRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TimerDefaults",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsBuilderRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsRefreshRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/StatusChipClassRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntimeStringConstants"
], function (ControllerRouteRuntime, ControllerViewStateRuntime, NavigationContracts, ModelContracts, TimerDefaults, AnalyticsBuilderRuntime, AnalyticsRefreshRuntime, StatusChipClassRuntime, JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;

    function clearRefreshTimer(oController) {
        if (oController._iAnalyticsRouteRefreshTimer) {
            clearInterval(oController._iAnalyticsRouteRefreshTimer);
            oController._iAnalyticsRouteRefreshTimer = null;
        }
    }

    function startRefreshTimer(oController) {
        var oStateModel = oController.getModel(ModelContracts.MODELS.STATE);
        var iIntervalMs = Number(oStateModel && oStateModel.getProperty("/timers/analyticsRefreshMs"))
            || Number((TimerDefaults.analyticsRefreshMs || {}).defaultValue)
            || 900000;
        clearRefreshTimer(oController);
        oController._iAnalyticsRouteRefreshTimer = setInterval(function () {
            var oView = oController && typeof oController.getView === TYPE_FUNCTION && oController.getView();
            if (!oController || oController.bIsDestroyed || (oView && oView.bIsDestroyed)) {
                clearRefreshTimer(oController);
                return;
            }
            if (typeof oController._loadAnalytics === TYPE_FUNCTION) {
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
            { name: NavigationContracts.ROUTES.DETAIL, handler: oController._onAnalyticsRouteLeave }
        ]);
    }

    function onAfterRendering(oController) {
        var oStateModel = oController && typeof oController[METHODS.GET_MODEL] === TYPE_FUNCTION && oController[METHODS.GET_MODEL](ModelContracts.MODELS.STATE);
        var sCurrentRouteName = String(oStateModel && typeof oStateModel[METHODS.GET_PROPERTY] === TYPE_FUNCTION && oStateModel[METHODS.GET_PROPERTY]("/currentRouteName") || "").trim();
        StatusChipClassRuntime.syncView(oController);
        if (!oController._bAnalyticsInitialRouteHandled && sCurrentRouteName === NavigationContracts.ROUTES.ANALYTICS) {
            Promise.resolve(oController._onAnalyticsMatched()).catch(function () {
                return null;
            });
        }
        if (sCurrentRouteName !== NavigationContracts.ROUTES.ANALYTICS) {
            clearRefreshTimer(oController);
        }
    }

    function onExit(oController) {
        clearRefreshTimer(oController);
        ControllerRouteRuntime.detachAllMatched(oController);
        if (oController._facade && typeof oController._facade[METHODS.DESTROY] === TYPE_FUNCTION) {
            oController._facade[METHODS.DESTROY]();
        }
        if (oController._oAnalyticsYearPicker && typeof oController._oAnalyticsYearPicker[METHODS.DESTROY] === TYPE_FUNCTION) {
            oController._oAnalyticsYearPicker[METHODS.DESTROY]();
        }
        if (oController._oAnalyticsReportDialog && typeof oController._oAnalyticsReportDialog[METHODS.DESTROY] === TYPE_FUNCTION) {
            oController._oAnalyticsReportDialog[METHODS.DESTROY]();
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
