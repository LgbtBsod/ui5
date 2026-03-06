sap.ui.define([
    "sap_ui5/util/SearchSmartControlCoordinator"
], function (SearchSmartControlCoordinator) {
    "use strict";

    function extractKey(oObject) {
        return SearchSmartControlCoordinator.extractChecklistId(oObject) || null;
    }

    function extractObjectsFromContexts(aContexts) {
        return SearchSmartControlCoordinator.extractObjectsFromContexts(aContexts);
    }

    function create(mRefs) {
        var oRefs = mRefs || {};
        var oSmartTable = oRefs.smartTable || null;
        var oSmartFilterBar = oRefs.smartFilterBar || null;
        var oInnerTable = oRefs.innerTable || (oSmartTable && oSmartTable.getTable ? oSmartTable.getTable() : null);
        function isSmartTableReady() {
            if (!oSmartTable) {
                return false;
            }
            if (typeof oSmartTable.isInitialised === "function") {
                return !!oSmartTable.isInitialised();
            }
            return true;
        }
        function isSmartFilterReady() {
            if (!oSmartFilterBar) {
                return true;
            }
            if (typeof oSmartFilterBar.isInitialised === "function") {
                return !!oSmartFilterBar.isInitialised();
            }
            return true;
        }
        function isReady() {
            return isSmartTableReady() && isSmartFilterReady();
        }
        function getItemsBinding() {
            return oInnerTable && oInnerTable.getBinding ? oInnerTable.getBinding("items") : null;
        }
        function isBusy() {
            if (oSmartTable && typeof oSmartTable.getBusy === "function" && oSmartTable.getBusy()) {
                return true;
            }
            if (oInnerTable && typeof oInnerTable.getBusy === "function" && oInnerTable.getBusy()) {
                return true;
            }
            return false;
        }
        function canTriggerRebind() {
            if (!oSmartTable || typeof oSmartTable.data !== "function") {
                return true;
            }
            var nNow = Date.now();
            var nLockUntil = Number(oSmartTable.data("__pcctRebindLockUntil") || 0);
            if (nLockUntil > nNow) {
                return false;
            }
            if (isBusy()) {
                oSmartTable.data("__pcctRebindLockUntil", nNow + 400);
                return false;
            }
            oSmartTable.data("__pcctRebindLockUntil", nNow + 250);
            return true;
        }

        return {
            isReady: isReady,
            rebindSearchTable: function () {
                if (isReady() && oSmartTable && oSmartTable.rebindTable && canTriggerRebind()) {
                    oSmartTable.rebindTable();
                    return true;
                }
                return false;
            },

            getSmartFilterData: function () {
                if (isSmartFilterReady() && oSmartFilterBar && oSmartFilterBar.getFilterData) {
                    return oSmartFilterBar.getFilterData(true) || {};
                }
                return {};
            },

            getSelectedRowKey: function () {
                if (!oInnerTable || !oInnerTable.getSelectedItem) {
                    return null;
                }
                var oSelectedItem = oInnerTable.getSelectedItem();
                var oCtx = oSelectedItem && oSelectedItem.getBindingContext && oSelectedItem.getBindingContext();
                var oObj = oCtx && oCtx.getObject && oCtx.getObject();
                return extractKey(oObj);
            },

            getVisibleRows: function () {
                if (!oInnerTable || !oInnerTable.getItems) {
                    return [];
                }
                return (oInnerTable.getItems() || []).map(function (oItem) {
                    var oCtx = oItem && oItem.getBindingContext && oItem.getBindingContext();
                    return oCtx && oCtx.getObject ? (oCtx.getObject() || {}) : null;
                }).filter(Boolean);
            },

            getSelectedRows: function () {
                if (!oInnerTable || !oInnerTable.getSelectedItems) {
                    return [];
                }
                return (oInnerTable.getSelectedItems() || []).map(function (oItem) {
                    var oCtx = oItem && oItem.getBindingContext && oItem.getBindingContext();
                    return oCtx && oCtx.getObject ? (oCtx.getObject() || null) : null;
                }).filter(Boolean);
            },

            getBoundRows: function (iLimit) {
                var oBinding = getItemsBinding();
                var iRequestedLength;
                if (!oBinding) {
                    return Promise.resolve(this.getVisibleRows());
                }
                iRequestedLength = Math.max(0, Number(iLimit) || Number(oBinding.getLength && oBinding.getLength()) || 0);
                if (!iRequestedLength) {
                    iRequestedLength = Number(oBinding.getLength && oBinding.getLength()) || 0;
                }
                if (typeof oBinding.requestContexts === "function") {
                    return oBinding.requestContexts(0, iRequestedLength || undefined).then(extractObjectsFromContexts);
                }
                if (typeof oBinding.getContexts === "function") {
                    return Promise.resolve(extractObjectsFromContexts(oBinding.getContexts(0, iRequestedLength || undefined)));
                }
                return Promise.resolve(this.getVisibleRows());
            },

            setTableBusy: function (bValue) {
                if (oSmartTable && oSmartTable.setBusy) {
                    oSmartTable.setBusy(!!bValue);
                } else if (oInnerTable && oInnerTable.setBusy) {
                    oInnerTable.setBusy(!!bValue);
                }
            }
        };
    }

    return {
        create: create
    };
});
