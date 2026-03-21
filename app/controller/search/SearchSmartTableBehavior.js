sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/internal/SearchViewLoadBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/internal/SearchFilterLifecycleBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/internal/SearchRequestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchRateProgress",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy"
], function (SearchViewLoadBehavior, SearchFilterLifecycleBehavior, SearchRequestRuntime, SearchSelectionRuntime, SearchViewportRuntime, SearchViewStateRuntime, SearchRateProgress, SearchCommandPolicy) {
    "use strict";

    function onSmartTableInitialise(oController, fnReadRows) {
        var oInnerTable = SearchSelectionRuntime.resolveSearchInnerTable(oController);

        SearchViewStateRuntime.setSmartTableReady(oController, true);
        if (!oInnerTable) {
            return;
        }
        SearchSelectionRuntime.configureSearchResultTable(oController, oInnerTable, true);
        SearchSelectionRuntime.bindSearchTableRuntime(oController, oInnerTable, function () {
            SearchViewportRuntime.bindSearchViewportRuntime(oController);
            SearchViewportRuntime.scheduleSearchViewportSync(oController, false);
        });
        if (oInnerTable.removeSelections) {
            oInnerTable.removeSelections(true);
        }
        if (oInnerTable.attachSelectionChange && typeof oController.onSearchTableSelectionChange === "function") {
            oInnerTable.attachSelectionChange(oController.onSearchTableSelectionChange, oController);
        }
        if (oInnerTable.attachItemPress && typeof oController.onSearchTableItemPress === "function") {
            oInnerTable.attachItemPress(oController.onSearchTableItemPress, oController);
        }
        SearchViewStateRuntime.syncSearchTableRequestWindow(oController);
        SearchRateProgress.wireTable(oController, oInnerTable);
        SearchViewportRuntime.bindSearchViewportRuntime(oController);
        SearchViewportRuntime.scheduleSearchViewportSync(oController, true);
        return SearchCommandPolicy.rebind(oController, { source: "smartTableInitialise" });
    }

    function onBeforeSmartTableRebind(oController, oEvent, fnReadRows) {
        SearchRequestRuntime.syncToolbarRequestInputs(oController);
        var mLoadHooks = SearchViewLoadBehavior.createSmartTableLoadHooks(oController, fnReadRows);
        var oBindingParams = oEvent && oEvent.getParameter && oEvent.getParameter("bindingParams");
        var oInnerTable = SearchSelectionRuntime.resolveSearchInnerTable(oController);

        SearchSelectionRuntime.configureSearchResultTable(oController, oInnerTable, true);
        SearchViewStateRuntime.syncSearchTableRequestWindow(oController);
        SearchViewStateRuntime.setTableBusy(oController, true);
        mLoadHooks.beginSearchLoadingFeedback();
        SearchViewportRuntime.scheduleSearchViewportSync(oController, false);
        mLoadHooks.bindPendingSearchLoad(oInnerTable);

        return Promise.resolve(SearchCommandPolicy.applyRebindPolicy(oController, {
            source: "beforeRebind",
            bindingParams: oBindingParams || {},
            state: SearchViewStateRuntime.readStateData(oController),
            onDataReceived: function (oDataEvent) {
                var oError = oDataEvent && oDataEvent.getParameter
                    && (oDataEvent.getParameter("error") || oDataEvent.getParameter("data") && oDataEvent.getParameter("data").error);
                mLoadHooks.settlePendingSearchLoad(oInnerTable, oError);
            }
        })).catch(function (oError) {
            mLoadHooks.settlePendingSearchLoad(oInnerTable, oError);
            return Promise.reject(oError);
        });
    }

    return {
        onSmartFilterInitialise: function (oController, fnApplyAnalyticsDrilldownIntent) {
            SearchFilterLifecycleBehavior.onSmartFilterInitialise(oController, fnApplyAnalyticsDrilldownIntent);
        },
        onSmartFilterChanged: function (oController) {
            SearchFilterLifecycleBehavior.onSmartFilterChanged(oController);
        },
        onSmartFilterClear: function (oController) {
            SearchFilterLifecycleBehavior.onSmartFilterClear(oController);
        },
        onMaxRowsChange: function (oController, oEvent) {
            SearchFilterLifecycleBehavior.onMaxRowsChange(oController, oEvent);
        },
        onBackendTopChange: function (oController, oEvent) {
            SearchFilterLifecycleBehavior.onBackendTopChange(oController, oEvent);
        },
        onSearchModeToggle: function (oController, oEvent) {
            SearchFilterLifecycleBehavior.onSearchModeToggle(oController, oEvent);
        },
        onBeforeSmartTableRebind: onBeforeSmartTableRebind,
        onSmartTableInitialise: onSmartTableInitialise
    };
});
