sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadingFeedbackRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewportRuntime"
], function (SearchLoadRuntime, SearchLoadingFeedbackRuntime, SearchSelectionRuntime, SearchViewportRuntime) {
    "use strict";

    function createSmartTableLoadHooks(oController, fnReadRows) {
        return {
            applyLoadError: function (sErrorMessage) {
                SearchLoadRuntime.applyLoadError(oController, sErrorMessage);
            },
            applyLoadSuccess: function (aRows) {
                SearchLoadRuntime.applyLoadSuccess(oController, aRows);
            },
            afterSuccess: function (oInnerTable) {
                SearchSelectionRuntime.syncSearchTableRuntimeState(oController, oInnerTable);
                SearchViewportRuntime.bindSearchViewportRuntime(oController);
                SearchViewportRuntime.scheduleSearchViewportSync(oController, true);
            },
            beginSearchLoadingFeedback: function () {
                SearchLoadingFeedbackRuntime.beginSearchLoadingFeedback(oController);
            },
            bindPendingSearchLoad: function (oInnerTable) {
                SearchLoadingFeedbackRuntime.bindPendingSearchLoad(oController, oInnerTable, {
                    applyLoadError: function (sErrorMessage) {
                        SearchLoadRuntime.applyLoadError(oController, sErrorMessage);
                    },
                    applyLoadSuccess: function (aRows) {
                        SearchLoadRuntime.applyLoadSuccess(oController, aRows);
                    },
                    readRows: function () {
                        return fnReadRows(oInnerTable);
                    },
                    afterSuccess: function () {
                        SearchSelectionRuntime.syncSearchTableRuntimeState(oController, oInnerTable);
                        SearchViewportRuntime.bindSearchViewportRuntime(oController);
                        SearchViewportRuntime.scheduleSearchViewportSync(oController, true);
                    }
                });
            },
            settlePendingSearchLoad: function (oInnerTable, oError) {
                SearchLoadingFeedbackRuntime.settlePendingSearchLoad(oController, { innerTable: oInnerTable, error: oError }, {
                    applyLoadError: function (sErrorMessage) {
                        SearchLoadRuntime.applyLoadError(oController, sErrorMessage);
                    },
                    applyLoadSuccess: function (aRows) {
                        SearchLoadRuntime.applyLoadSuccess(oController, aRows);
                    },
                    readRows: function () {
                        return fnReadRows(oInnerTable);
                    },
                    afterSuccess: function () {
                        SearchSelectionRuntime.syncSearchTableRuntimeState(oController, oInnerTable);
                        SearchViewportRuntime.bindSearchViewportRuntime(oController);
                        SearchViewportRuntime.scheduleSearchViewportSync(oController, true);
                    }
                });
            }
        };
    }

    return {
        beginSearchLoadingFeedback: SearchLoadingFeedbackRuntime.beginSearchLoadingFeedback,
        createSmartTableLoadHooks: createSmartTableLoadHooks,
        resetTransientFeedback: SearchLoadingFeedbackRuntime.resetTransientFeedback
    };
});
