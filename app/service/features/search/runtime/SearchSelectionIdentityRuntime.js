sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ChecklistIdentity"
], function (ChecklistIdentity) {
    "use strict";

    function extractChecklistIdFromListItem(oListItem) {
        var oCtx = oListItem && oListItem.getBindingContext && oListItem.getBindingContext();
        var oObject = oCtx && oCtx.getObject && oCtx.getObject();
        return ChecklistIdentity.extractChecklistId(oObject);
    }

    function extractChecklistDisplayIdFromListItem(oListItem) {
        var oCtx = oListItem && oListItem.getBindingContext && oListItem.getBindingContext();
        var oObject = oCtx && oCtx.getObject && oCtx.getObject();
        return ChecklistIdentity.extractChecklistDisplayId(oObject);
    }

    function extractSelectedRowIds(oEvent, oTable) {
        var aSelectedContexts = [];
        var aRowContexts = [];
        var aIds = [];
        var aSelectedItems = oTable && oTable.getSelectedItems ? (oTable.getSelectedItems() || []) : [];
        if (oEvent && typeof oEvent.getParameter === "function") {
            aSelectedContexts = oEvent.getParameter("selectedContexts") || [];
            if (oEvent.getParameter("rowContext")) {
                aRowContexts.push(oEvent.getParameter("rowContext"));
            }
        }
        aIds = (aSelectedContexts || []).map(function (oCtx) {
            return ChecklistIdentity.extractChecklistId(oCtx && oCtx.getObject && oCtx.getObject());
        });
        aIds = aIds.concat((aRowContexts || []).map(function (oCtx) {
            return ChecklistIdentity.extractChecklistId(oCtx && oCtx.getObject && oCtx.getObject());
        }));
        aIds = aIds.concat(aSelectedItems.map(extractChecklistIdFromListItem));
        return ChecklistIdentity.normalizeChecklistIds(aIds);
    }

    function extractSelectedRowId(oEvent, oTable) {
        return extractSelectedRowIds(oEvent, oTable)[0] || "";
    }

    function extractSelectedRowDisplayId(oEvent, oTable) {
        var oSelectedItems = oTable && oTable.getSelectedItems ? (oTable.getSelectedItems() || []) : [];
        var oListItem = oSelectedItems[0] || null;
        return String((oListItem && extractChecklistDisplayIdFromListItem(oListItem)) || "").trim();
    }

    function resolveSelectedRowIdsFromInnerTable(oInnerTable) {
        var aSelectedItems = oInnerTable && oInnerTable.getSelectedItems ? (oInnerTable.getSelectedItems() || []) : [];
        return ChecklistIdentity.normalizeChecklistIds(aSelectedItems.map(extractChecklistIdFromListItem));
    }

    function resolveSelectedRowDisplayIdFromInnerTable(oInnerTable) {
        var aSelectedItems = oInnerTable && oInnerTable.getSelectedItems ? (oInnerTable.getSelectedItems() || []) : [];
        return String((aSelectedItems[0] && extractChecklistDisplayIdFromListItem(aSelectedItems[0])) || "").trim();
    }

    return {
        extractSelectedRowDisplayId: extractSelectedRowDisplayId,
        extractSelectedRowId: extractSelectedRowId,
        extractSelectedRowIds: extractSelectedRowIds,
        resolveSelectedRowDisplayIdFromInnerTable: resolveSelectedRowDisplayIdFromInnerTable,
        resolveSelectedRowIdsFromInnerTable: resolveSelectedRowIdsFromInnerTable
    };
});
