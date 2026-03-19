sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchViewLoadBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSmartTableRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchRateProgress",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy"
], function (SearchViewLoadBehavior, SearchSmartTableRuntime, SearchSelectionRuntime, SearchViewportRuntime, SearchViewStateRuntime, SearchRateProgress, SearchCommandPolicy) {
    "use strict";

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
        var mLoadHooks = SearchViewLoadBehavior.createSmartTableLoadHooks(oController, fnReadRows);
        return SearchSmartTableRuntime.onBeforeSmartTableRebind(oController, oEvent, {
            applyRebindPolicy: function (mInput) {
                return SearchCommandPolicy.applyRebindPolicy(oController, mInput);
            },
            beginSearchLoadingFeedback: mLoadHooks.beginSearchLoadingFeedback,
            bindPendingSearchLoad: function (oInnerTable) {
                return mLoadHooks.bindPendingSearchLoad(oInnerTable);
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
            settlePendingSearchLoad: mLoadHooks.settlePendingSearchLoad,
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
