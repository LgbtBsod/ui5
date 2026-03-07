sap.ui.define([
    "checklist/app/service/domain/analytics/AnalyticsFacade",
    "checklist/app/service/domain/analytics/AnalyticsPayloadNormalizer",
    "checklist/app/service/framework/NavigationIntentService",
    "checklist/app/service/framework/ControllerCtxRuntime",
    "checklist/app/service/framework/FacadeCommandRuntime",
    "checklist/app/service/framework/ControllerRouteRuntime",
    "checklist/app/service/framework/ControllerViewStateRuntime"
], function (AnalyticsFacade, AnalyticsPayloadNormalizer, NavigationIntentService, ControllerCtxRuntime, FacadeCommandRuntime, ControllerRouteRuntime, ControllerViewStateRuntime) {
    "use strict";

    function buildInitialViewState() {
        return {
            busy: false,
            error: "",
            analytics: AnalyticsPayloadNormalizer.createEmptyDashboard()
        };
    }

    return {
        onInit: function () {
            this._facade = new AnalyticsFacade();
            ControllerViewStateRuntime.initModel(this, buildInitialViewState);
            ControllerRouteRuntime.attachMatched(this, [
                { name: "analytics", handler: this._onAnalyticsMatched }
            ]);
        },

        onExit: function () {
            ControllerRouteRuntime.detachAllMatched(this);
            this._facade = null;
        },

        _loadAnalytics: function (sReason) {
            ControllerViewStateRuntime.setMany(this, {
                "/busy": true,
                "/error": ""
            });
            return FacadeCommandRuntime.executeRaw(
                this,
                this._facade,
                "load",
                { reason: sReason || "manual" },
                ControllerCtxRuntime.buildDefault(this)
            );
        },

        _onAnalyticsMatched: function () {
            return this._loadAnalytics("routeMatched");
        },

        onRefreshAnalytics: function () {
            return this._loadAnalytics("manualRefresh");
        },

        onCloseAnalytics: function () {
            NavigationIntentService.navigateBackFromAnalytics(this);
        }
    };
});
