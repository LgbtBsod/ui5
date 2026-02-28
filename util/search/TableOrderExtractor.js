sap.ui.define([], function () {
    "use strict";

    return {
        getDisplayedRootKeysInOrder: function (oTable) {
            var aItems = (oTable && oTable.getItems && oTable.getItems()) || [];
            return aItems.map(function (oItem) {
                var oCtx = oItem.getBindingContext && oItem.getBindingContext();
                var oObj = oCtx && oCtx.getObject && oCtx.getObject();
                return (oObj && (oObj.Key || oObj.RootKey || oObj.Id)) || null;
            }).filter(Boolean);
        }
    };
});
