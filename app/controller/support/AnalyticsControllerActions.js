sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "checklist/app/service/framework/CtxFactory",
    "checklist/app/service/domain/analytics/AnalyticsFacade",
    "checklist/app/service/domain/analytics/AnalyticsPayloadNormalizer",
    "checklist/app/infra/navigation/WorkspaceRouteNavigation",
    "checklist/app/controller/support/ControllerModelWriteSupport"
], function (JSONModel, CtxFactory, AnalyticsFacade, AnalyticsPayloadNormalizer, WorkspaceRouteNavigation, ControllerModelWriteSupport) {
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
            this.attachRouteMatched("analytics", this._onAnalyticsMatched);
        },

        onExit: function () {
            if (this.detachAllRouteMatched) {
                this.detachAllRouteMatched();
            }
            this._facade = null;
        },

        _ctx: function () {
            return CtxFactory.buildCtx(this, {});
        },

        _run: function (sMethod, mInput) {
            return this.executeFacadeMethod(this._facade, sMethod, mInput, this._ctx());
        },

        _loadAnalytics: function (sReason) {
            ControllerModelWriteSupport.setMany(this, "view", {
                "/busy": true,
                "/error": ""
            });
            return this._run("load", { reason: sReason || "manual" });
        },

        _onAnalyticsMatched: function () {
            return this._loadAnalytics("routeMatched");
        },

        onRefreshAnalytics: function () {
            return this._loadAnalytics("manualRefresh");
        },

        onCloseAnalytics: function () {
            WorkspaceRouteNavigation.navigateBackFromAnalytics(this);
        }
    };
});
