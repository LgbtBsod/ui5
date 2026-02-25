sap.ui.define([], function () {
    "use strict";

    function normalizeAnalytics(oAnalytics) {
        var oValue = oAnalytics || {};
        if (!oValue.source) {
            oValue.source = "fallback";
        }
        return oValue;
    }

    function runAnalyticsLoadFlow(mArgs) {
        var oViewModel = mArgs.viewModel;
        var fnLoad = mArgs.loadAnalytics;

        oViewModel.setProperty("/analyticsBusy", true);
        oViewModel.setProperty("/analyticsError", "");

        return fnLoad().then(function (oAnalytics) {
            oViewModel.setProperty("/analytics", normalizeAnalytics(oAnalytics));
            return oAnalytics;
        }).catch(function (oError) {
            oViewModel.setProperty("/analyticsError", (oError && oError.message) || "");
            return null;
        }).finally(function () {
            oViewModel.setProperty("/analyticsBusy", false);
        });
    }

    function openDialogLifecycle(mArgs) {
        if (!mArgs || typeof mArgs.openDialog !== "function") {
            return;
        }
        mArgs.openDialog(mArgs.dialog, mArgs.runLoad);
    }

    function closeDialogLifecycle(mArgs) {
        if (!mArgs || typeof mArgs.closeDialog !== "function") {
            return;
        }
        mArgs.closeDialog(mArgs.dialog);
    }

    return {
        normalizeAnalytics: normalizeAnalytics,
        runAnalyticsLoadFlow: runAnalyticsLoadFlow,
        openDialogLifecycle: openDialogLifecycle,
        closeDialogLifecycle: closeDialogLifecycle
    };
});
