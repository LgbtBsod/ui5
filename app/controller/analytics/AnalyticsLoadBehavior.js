sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsBuilderRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsYearRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsRefreshRuntime"
], function (AnalyticsBuilderRuntime, AnalyticsYearRuntime, AnalyticsLoadRuntime, AnalyticsRefreshRuntime) {
    "use strict";

    function loadAnalytics(oController, sReason, sSelectedYearPath, sCompareYearPath, fnBuildCtx) {
        return AnalyticsLoadRuntime.loadAnalytics(oController, sReason, {
            applyBuilderSelection: function (oTarget) {
                AnalyticsBuilderRuntime.applyBuilderSelection(oTarget);
            },
            applyComparisonMetricSelection: function (oTarget) {
                AnalyticsBuilderRuntime.applyComparisonMetricSelection(oTarget);
            },
            buildCompareYearOptions: function (oTarget) {
                return AnalyticsYearRuntime.buildCompareYearOptions(oTarget, sSelectedYearPath, sCompareYearPath);
            },
            buildCtx: fnBuildCtx,
            buildYearOptions: function (oTarget) {
                return AnalyticsYearRuntime.buildYearOptions(oTarget, sSelectedYearPath, sCompareYearPath);
            },
            setCompareYearValidation: function (oTarget, sState, sText) {
                oTarget._setCompareYearValidation(sState, sText);
            },
            syncAnalyticsContextHints: function (oTarget) {
                AnalyticsBuilderRuntime.syncAnalyticsContextHints(oTarget);
            },
            syncCompareYearDefaults: function (oTarget, sSelectedYear) {
                return AnalyticsYearRuntime.syncCompareYearDefaults(oTarget, sSelectedYear, sSelectedYearPath, sCompareYearPath);
            }
        });
    }

    function pollRefreshStateUntilSettled(oController, iAttemptsLeft, fnBuildCtx) {
        var oCtx = fnBuildCtx(oController);
        return AnalyticsRefreshRuntime.pollRefreshStateUntilSettled(oController, iAttemptsLeft, function () {
            return oCtx && oCtx.analytics && oCtx.analytics.fetchRefreshState ? oCtx.analytics.fetchRefreshState() : null;
        });
    }

    function onAnalyticsMatched(oController, fnLoadAnalytics) {
        oController._bAnalyticsInitialRouteHandled = true;
        return fnLoadAnalytics(oController, "routeMatched").then(function (vResult) {
            if (oController._bAnalyticsRouteActive && typeof oController._startAnalyticsRefreshTimer === "function") {
                oController._startAnalyticsRefreshTimer();
            }
            return vResult;
        });
    }

    return {
        loadAnalytics: loadAnalytics,
        onAnalyticsMatched: onAnalyticsMatched,
        pollRefreshStateUntilSettled: pollRefreshStateUntilSettled
    };
});
