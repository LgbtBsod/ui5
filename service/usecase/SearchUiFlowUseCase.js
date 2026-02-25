sap.ui.define([], function () {
    "use strict";

    function extractIdFromListSelectionEvent(oEvent) {
        var oListItem = oEvent.getParameter("listItem");
        var oSource = oEvent.getSource();

        if (!oListItem && oSource && typeof oSource.getSelectedItem === "function") {
            oListItem = oSource.getSelectedItem();
        }

        var oCtx = oListItem ? oListItem.getBindingContext("data") : null;
        var oChecklist = oCtx ? oCtx.getObject() : null;
        return String((oChecklist && oChecklist.root && oChecklist.root.id) || "").trim();
    }

    function resolveExportEntityFromMenuEvent(oEvent, sDefaultEntity) {
        var oItem = oEvent.getParameter("item");
        var sEntity = oItem ? oItem.data("entity") : sDefaultEntity;
        return sEntity || sDefaultEntity;
    }

    function hasDialog(oDialog) {
        return !!oDialog;
    }

    return {
        extractIdFromListSelectionEvent: extractIdFromListSelectionEvent,
        resolveExportEntityFromMenuEvent: resolveExportEntityFromMenuEvent,
        hasDialog: hasDialog
    };
});
