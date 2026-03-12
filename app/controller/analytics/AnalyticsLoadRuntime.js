sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FacadeCommandRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsContracts"
], function (ControllerViewStateRuntime, ModelStateRuntime, FacadeCommandRuntime, StatePaths, AnalyticsContracts) {
    "use strict";

    return {
        loadAnalytics: function (oController, sReason, mHooks) {
            var sSelectedYear = String(ControllerViewStateRuntime.get(oController, "/selectedYear", "") || "").trim();
            var sCompareYear = String(ControllerViewStateRuntime.get(oController, "/compareYear", "") || "").trim();
            var sSelectedSource = String(ControllerViewStateRuntime.get(oController, "/selectedSource", AnalyticsContracts.SOURCES.ALL) || AnalyticsContracts.SOURCES.ALL).trim().toUpperCase();

            ModelStateRuntime.write(oController, "state", StatePaths.UI_BUSY_ANALYTICS, true);
            ModelStateRuntime.write(oController, "state", StatePaths.READINESS_ANALYTICS, {
                status: "loading",
                ready: false,
                readyAt: "",
                error: ""
            });
            ControllerViewStateRuntime.setMany(oController, {
                "/busy": true,
                "/error": ""
            });

            return FacadeCommandRuntime.executeRaw(
                oController,
                oController._facade,
                "load",
                {
                    reason: sReason || "manual",
                    selectedYear: Number(sSelectedYear) || 0,
                    compareYear: Number(sCompareYear) || 0,
                    selectedSource: sSelectedSource
                },
                mHooks.buildCtx(oController)
            ).then(function (oResult) {
                var oAnalytics = ControllerViewStateRuntime.get(oController, "/analytics", {}) || {};

                if (Array.isArray(oAnalytics.availableYears) && oAnalytics.availableYears.length) {
                    ControllerViewStateRuntime.set(oController, "/availableYears", oAnalytics.availableYears);
                }
                if (oAnalytics.selectedYear) {
                    ControllerViewStateRuntime.set(oController, "/selectedYear", String(oAnalytics.selectedYear));
                }
                if (oAnalytics.compareYear) {
                    ControllerViewStateRuntime.set(oController, "/compareYear", String(oAnalytics.compareYear));
                } else if (oAnalytics.selectedYear) {
                    mHooks.syncCompareYearDefaults(oController, String(oAnalytics.selectedYear));
                }
                if (oAnalytics.source) {
                    ControllerViewStateRuntime.set(oController, "/selectedSource", String(oAnalytics.source));
                }
                if (oAnalytics.refreshState) {
                    ControllerViewStateRuntime.set(oController, "/refreshState", oAnalytics.refreshState);
                }
                ControllerViewStateRuntime.set(oController, "/availableYears", mHooks.buildYearOptions(oController));
                ControllerViewStateRuntime.set(oController, "/compareYearOptions", mHooks.buildCompareYearOptions(oController));
                mHooks.setCompareYearValidation(oController, "None", "");
                mHooks.applyComparisonMetricSelection(oController);
                mHooks.applyBuilderSelection(oController);
                mHooks.syncAnalyticsContextHints(oController);
                return oResult;
            });
        }
    };
});
