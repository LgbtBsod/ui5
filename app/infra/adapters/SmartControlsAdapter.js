sap.ui.define([
"PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSmartControlCoordinator",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/JsRuntime"
], function (SearchSmartControlCoordinator, JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;

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
            if (typeof oSmartTable.isInitialised === TYPE_FUNCTION) {
                return !!oSmartTable.isInitialised();
            }
            return true;
        }
        function isSmartFilterReady() {
            if (!oSmartFilterBar) {
                return true;
            }
            if (typeof oSmartFilterBar.isInitialised === TYPE_FUNCTION) {
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
            if (oSmartTable && typeof oSmartTable.getBusy === TYPE_FUNCTION && oSmartTable.getBusy()) {
                return true;
            }
            if (oInnerTable && typeof oInnerTable.getBusy === TYPE_FUNCTION && oInnerTable.getBusy()) {
                return true;
            }
            return false;
        }
        var fnDebouncedRebind = SearchSmartControlCoordinator.createDebouncedRebind(function () {
            if (isReady() && !isBusy() && oSmartTable && oSmartTable.rebindTable) {
                oSmartTable.rebindTable();
            }
        }, 80);

        return {
            isReady: isReady,
            rebindSearchTable: function () {
                if (!isReady() || !oSmartTable || !oSmartTable.rebindTable || isBusy()) {
                    return false;
                }
                fnDebouncedRebind();
                return true;
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
                if (typeof oBinding[METHODS.REQUEST_CONTEXTS] === TYPE_FUNCTION) {
                    return oBinding[METHODS.REQUEST_CONTEXTS](0, iRequestedLength || undefined).then(extractObjectsFromContexts);
                }
                if (typeof oBinding[METHODS.GET_CONTEXTS] === TYPE_FUNCTION) {
                    return Promise.resolve(extractObjectsFromContexts(oBinding[METHODS.GET_CONTEXTS](0, iRequestedLength || undefined)));
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
