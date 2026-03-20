sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadingFeedbackRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchReturnRediscoveryRuntime"
], function (SearchLoadRuntime, SearchLoadingFeedbackRuntime, SearchSelectionRuntime, SearchViewportRuntime, SearchReturnRediscoveryRuntime) {
    "use strict";

    function afterSearchLoadSuccess(oController, oInnerTable) {
        SearchSelectionRuntime.syncSearchTableRuntimeState(oController, oInnerTable);
        SearchReturnRediscoveryRuntime.applyAfterSearchSuccess(oController, oInnerTable);
        SearchViewportRuntime.bindSearchViewportRuntime(oController);
        SearchViewportRuntime.scheduleSearchViewportSync(oController, true);
    }

    function createSmartTableLoadHooks(oController, fnReadRows) {
        return {
            applyLoadError: function (sErrorMessage) {
                SearchLoadRuntime.applyLoadError(oController, sErrorMessage);
            },
            applyLoadSuccess: function (aRows) {
                SearchLoadRuntime.applyLoadSuccess(oController, aRows);
            },
            afterSuccess: function (oInnerTable) {
                afterSearchLoadSuccess(oController, oInnerTable);
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
                        afterSearchLoadSuccess(oController, oInnerTable);
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
                        afterSearchLoadSuccess(oController, oInnerTable);
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
