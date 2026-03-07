sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "checklist/app/service/domain/analytics/AnalyticsFacade",
    "checklist/app/service/domain/analytics/AnalyticsPayloadNormalizer",
    "checklist/app/service/framework/NavigationIntentService",
    "checklist/app/controller/support/ControllerModelWriteSupport",
    "checklist/app/service/framework/CtxFactory",
    "checklist/app/service/framework/FacadeCommandRuntime",
    "checklist/app/service/framework/ControllerRouteRuntime"
], function (JSONModel, AnalyticsFacade, AnalyticsPayloadNormalizer, NavigationIntentService, ControllerModelWriteSupport, CtxFactory, FacadeCommandRuntime, ControllerRouteRuntime) {
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
            this.setModel(new JSONModel(buildInitialViewState()), "view");
            ControllerRouteRuntime.attachMatched(this, [
                { name: "analytics", handler: this._onAnalyticsMatched }
            ]);
        },

        onExit: function () {
            ControllerRouteRuntime.detachAllMatched(this);
            this._facade = null;
        },

        _loadAnalytics: function (sReason) {
            ControllerModelWriteSupport.setMany(this, "view", {
                "/busy": true,
                "/error": ""
            });
            return FacadeCommandRuntime.executeRaw(
                this,
                this._facade,
                "load",
                { reason: sReason || "manual" },
                CtxFactory.buildCtx(this, {})
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
