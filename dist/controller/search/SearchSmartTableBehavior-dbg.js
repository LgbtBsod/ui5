sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSmartTableRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadingFeedbackRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchRateProgress",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy"
], function (SearchSmartTableRuntime, SearchSelectionRuntime, SearchViewportRuntime, SearchViewStateRuntime, SearchLoadingFeedbackRuntime, SearchLoadRuntime, SearchRateProgress, SearchCommandPolicy) {
    "use strict";

    function createSearchLoadHooks(oController, oInnerTable, fnReadRows) {
        return {
            applyLoadError: function (sErrorMessage) {
                SearchLoadRuntime.applyLoadError(oController, sErrorMessage);
            },
            applyLoadSuccess: function (aRows) {
                SearchLoadRuntime.applyLoadSuccess(oController, aRows);
            },
            readRows: fnReadRows,
            afterSuccess: function () {
                SearchSelectionRuntime.syncSearchTableRuntimeState(oController, oInnerTable);
                SearchViewportRuntime.bindSearchViewportRuntime(oController);
                SearchViewportRuntime.scheduleSearchViewportSync(oController, true);
            }
        };
    }

    function onSmartTableInitialise(oController, fnReadRows) {
        return SearchSmartTableRuntime.onSmartTableInitialise(oController, {
            bindTableRuntime: function (oInnerTable, fnAfterBind) {
                SearchSelectionRuntime.bindSearchTableRuntime(oController, oInnerTable, fnAfterBind);
            },
            bindViewportRuntime: function () {
                SearchViewportRuntime.bindSearchViewportRuntime(oController);
            },
            configureResultTable: function (oInnerTable, bForce) {
                SearchSelectionRuntime.configureSearchResultTable(oController, oInnerTable, bForce);
            },
            onItemPress: oController.onSearchTableItemPress,
            onSelectionChange: oController.onSearchTableSelectionChange,
            refreshTableIfNeeded: function (sSource) {
                return SearchCommandPolicy.rebind(oController, { source: sSource });
            },
            resolveInnerTable: function () {
                return SearchSelectionRuntime.resolveSearchInnerTable(oController);
            },
            scheduleViewportSync: function (bImmediate) {
                SearchViewportRuntime.scheduleSearchViewportSync(oController, bImmediate);
            },
            syncRequestWindow: function () {
                SearchViewStateRuntime.syncSearchTableRequestWindow(oController);
            },
            wireRateProgress: function (oInnerTable) {
                SearchRateProgress.wireTable(oController, oInnerTable);
            }
        });
    }

    function onBeforeSmartTableRebind(oController, oEvent, fnReadRows) {
        return SearchSmartTableRuntime.onBeforeSmartTableRebind(oController, oEvent, {
            applyRebindPolicy: function (mInput) {
                return SearchCommandPolicy.applyRebindPolicy(oController, mInput);
            },
            beginSearchLoadingFeedback: function () {
                SearchLoadingFeedbackRuntime.beginSearchLoadingFeedback(oController);
            },
            bindPendingSearchLoad: function (oInnerTable) {
                SearchLoadingFeedbackRuntime.bindPendingSearchLoad(
                    oController,
                    oInnerTable,
                    createSearchLoadHooks(oController, oInnerTable, function () {
                        return fnReadRows(oInnerTable);
                    })
                );
            },
            configureResultTable: function (oInnerTable, bForce) {
                SearchSelectionRuntime.configureSearchResultTable(oController, oInnerTable, bForce);
            },
            resolveInnerTable: function () {
                return SearchSelectionRuntime.resolveSearchInnerTable(oController);
            },
            scheduleViewportSync: function (bImmediate) {
                SearchViewportRuntime.scheduleSearchViewportSync(oController, bImmediate);
            },
            settlePendingSearchLoad: function (oInnerTable, oError) {
                SearchLoadingFeedbackRuntime.settlePendingSearchLoad(
                    oController,
                    { innerTable: oInnerTable, error: oError },
                    createSearchLoadHooks(oController, oInnerTable, function () {
                        return fnReadRows(oInnerTable);
                    })
                );
            },
            syncRequestWindow: function () {
                SearchViewStateRuntime.syncSearchTableRequestWindow(oController);
            }
        });
    }

    return {
        onBeforeSmartTableRebind: onBeforeSmartTableRebind,
        onSmartTableInitialise: onSmartTableInitialise
    };
});
