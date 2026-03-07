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
        var iCurrentYear = new Date().getFullYear();
        return {
            busy: false,
            error: "",
            selectedYear: String(iCurrentYear),
            availableYears: [{ key: String(iCurrentYear), text: String(iCurrentYear) }],
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
            var sSelectedYear = String(ControllerViewStateRuntime.get(this, "/selectedYear", "") || "").trim();
            ControllerViewStateRuntime.setMany(this, {
                "/busy": true,
                "/error": ""
            });
            return FacadeCommandRuntime.executeRaw(
                this,
                this._facade,
                "load",
                {
                    reason: sReason || "manual",
                    selectedYear: Number(sSelectedYear) || 0
                },
                ControllerCtxRuntime.buildDefault(this)
            ).then(function (oResult) {
                var oAnalytics = ControllerViewStateRuntime.get(this, "/analytics", {}) || {};
                if (Array.isArray(oAnalytics.availableYears) && oAnalytics.availableYears.length) {
                    ControllerViewStateRuntime.set(this, "/availableYears", oAnalytics.availableYears);
                }
                if (oAnalytics.selectedYear) {
                    ControllerViewStateRuntime.set(this, "/selectedYear", String(oAnalytics.selectedYear));
                }
                return oResult;
            }.bind(this));
        },

        _onAnalyticsMatched: function () {
            return this._loadAnalytics("routeMatched");
        },

        onRefreshAnalytics: function () {
            return this._loadAnalytics("manualRefresh");
        },

        onSelectAnalyticsYear: function (oEvent) {
            var sYear = String(oEvent && oEvent.getParameter && oEvent.getParameter("selectedItem") && oEvent.getParameter("selectedItem").getKey() || oEvent && oEvent.getParameter && oEvent.getParameter("selectedKey") || "").trim();
            if (!sYear) {
                return Promise.resolve();
            }
            ControllerViewStateRuntime.set(this, "/selectedYear", sYear);
            return this._loadAnalytics("yearChanged");
        },

        onCloseAnalytics: function () {
            NavigationIntentService.navigateBackFromAnalytics(this);
        }
    };
});
