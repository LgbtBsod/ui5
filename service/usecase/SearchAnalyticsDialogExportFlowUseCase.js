sap.ui.define([], function () {
    "use strict";

    function openAnalyticsDialog(oDialog, fnLoadAnalytics) {
        if (!oDialog) {
            return;
        }
        oDialog.open();
        if (typeof fnLoadAnalytics === "function") {
            fnLoadAnalytics();
        }
    }

    function closeAnalyticsDialog(oDialog) {
        if (!oDialog) {
            return;
        }
        oDialog.close();
    }

    function runExportFlow(mArgs) {
        return mArgs.runWithLoading(function () {
            return mArgs.buildExportPromise().then(function (oResult) {
                var aRows = (oResult && oResult.rows) || [];
                if (!aRows.length) {
                    mArgs.onEmpty();
                    return null;
                }
                mArgs.onSuccess(aRows);
                return aRows;
            }).catch(function (oError) {
                mArgs.onError(oError);
                return null;
            });
        });
    }

    return {
        openAnalyticsDialog: openAnalyticsDialog,
        closeAnalyticsDialog: closeAnalyticsDialog,
        runExportFlow: runExportFlow
    };
});
