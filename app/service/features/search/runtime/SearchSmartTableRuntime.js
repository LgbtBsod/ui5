sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/OperationSourceContracts"
], function (ControllerViewStateRuntime, ControllerModelRuntime, OperationSourceContracts) {
    "use strict";

    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;

    function onSmartTableInitialise(oController, mHooks) {
        var oInnerTable = mHooks && mHooks.resolveInnerTable && mHooks.resolveInnerTable();
        ControllerViewStateRuntime.set(oController, "/smartTableReady", true);
        if (!oInnerTable) {
            return;
        }
        if (typeof mHooks.configureResultTable === "function") {
            mHooks.configureResultTable(oInnerTable, true);
        }
        if (typeof mHooks.bindTableRuntime === "function") {
            mHooks.bindTableRuntime(oInnerTable, function () {
                if (typeof mHooks.bindViewportRuntime === "function") {
                    mHooks.bindViewportRuntime();
                }
                if (typeof mHooks.scheduleViewportSync === "function") {
                    mHooks.scheduleViewportSync(false);
                }
            });
        }
        if (oInnerTable.removeSelections) {
            oInnerTable.removeSelections(true);
        }
        if (oInnerTable.attachSelectionChange && typeof mHooks.onSelectionChange === "function") {
            oInnerTable.attachSelectionChange(mHooks.onSelectionChange, oController);
        }
        if (oInnerTable.attachItemPress && typeof mHooks.onItemPress === "function") {
            oInnerTable.attachItemPress(mHooks.onItemPress, oController);
        }
        if (typeof mHooks.syncRequestWindow === "function") {
            mHooks.syncRequestWindow();
        }
        if (typeof mHooks.wireRateProgress === "function") {
            mHooks.wireRateProgress(oInnerTable);
        }
        if (typeof mHooks.bindViewportRuntime === "function") {
            mHooks.bindViewportRuntime();
        }
        if (typeof mHooks.scheduleViewportSync === "function") {
            mHooks.scheduleViewportSync(true);
        }
        if (typeof mHooks.refreshTableIfNeeded === "function") {
            mHooks.refreshTableIfNeeded("smartTableInitialise");
        }
    }

    function onBeforeSmartTableRebind(oController, oEvent, mHooks) {
        var oBindingParams = oEvent && oEvent.getParameter && oEvent.getParameter("bindingParams");
        var oStateModel = ControllerModelRuntime.state(oController);
        var oInnerTable = mHooks && mHooks.resolveInnerTable && mHooks.resolveInnerTable();
        if (typeof mHooks.configureResultTable === "function") {
            mHooks.configureResultTable(oInnerTable, true);
        }
        if (typeof mHooks.syncRequestWindow === "function") {
            mHooks.syncRequestWindow();
        }
        ControllerViewStateRuntime.set(oController, "/tableBusy", true);
        if (typeof mHooks.beginSearchLoadingFeedback === "function") {
            mHooks.beginSearchLoadingFeedback();
        }
        if (typeof mHooks.scheduleViewportSync === "function") {
            mHooks.scheduleViewportSync(false);
        }
        if (typeof mHooks.bindPendingSearchLoad === "function") {
            mHooks.bindPendingSearchLoad(oInnerTable);
        }
        return Promise.resolve(mHooks.applyRebindPolicy({
            source: SEARCH_SOURCES.BEFORE_REBIND,
            bindingParams: oBindingParams || {},
            state: (oStateModel && oStateModel.getData && oStateModel.getData()) || {},
            onDataReceived: function (oDataEvent) {
                var oError = oDataEvent && oDataEvent.getParameter
                    && (oDataEvent.getParameter("error") || oDataEvent.getParameter("data") && oDataEvent.getParameter("data").error);
                if (typeof mHooks.settlePendingSearchLoad === "function") {
                    mHooks.settlePendingSearchLoad(oInnerTable, oError);
                }
            }
        })).catch(function (oError) {
            if (typeof mHooks.settlePendingSearchLoad === "function") {
                mHooks.settlePendingSearchLoad(oInnerTable, oError);
            }
            return Promise.reject(oError);
        });
    }

    return {
        onBeforeSmartTableRebind: onBeforeSmartTableRebind,
        onSmartTableInitialise: onSmartTableInitialise
    };
});
