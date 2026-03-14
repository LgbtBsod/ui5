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
        var aListItems = [];
        var aSelectedContexts = [];
        var aRowContexts = [];
        var aIds = [];
        if (oEvent && typeof oEvent.getParameter === "function") {
            aListItems = oEvent.getParameter("listItems") || [];
            if (!Array.isArray(aListItems) || !aListItems.length) {
                [
                    oEvent.getParameter("listItem"),
                    oEvent.getParameter("item"),
                    oEvent.getParameter("selectedItem")
                ].forEach(function (oCandidate) {
                    if (oCandidate) {
                        aListItems.push(oCandidate);
                    }
                });
            }
            aSelectedContexts = oEvent.getParameter("selectedContexts") || [];
            if (oEvent.getParameter("rowContext")) {
                aRowContexts.push(oEvent.getParameter("rowContext"));
            }
        }
        aIds = (aListItems || []).map(extractChecklistIdFromListItem);
        aIds = aIds.concat((aSelectedContexts || []).map(function (oCtx) {
            return ChecklistIdentity.extractChecklistId(oCtx && oCtx.getObject && oCtx.getObject());
        }));
        aIds = aIds.concat((aRowContexts || []).map(function (oCtx) {
            return ChecklistIdentity.extractChecklistId(oCtx && oCtx.getObject && oCtx.getObject());
        }));
        aIds = aIds.concat(
            ((oTable && oTable.getSelectedItems && oTable.getSelectedItems()) || []).map(extractChecklistIdFromListItem)
        );
        return ChecklistIdentity.normalizeChecklistIds(aIds);
    }

    function extractSelectedRowId(oEvent, oTable) {
        return extractSelectedRowIds(oEvent, oTable)[0] || "";
    }

    function extractSelectedRowDisplayId(oEvent, oTable) {
        var oListItem = null;
        var oSelectedItems = oTable && oTable.getSelectedItems ? (oTable.getSelectedItems() || []) : [];
        if (oEvent && typeof oEvent.getParameter === "function") {
            oListItem = oEvent.getParameter("listItem")
                || oEvent.getParameter("item")
                || oEvent.getParameter("selectedItem")
                || ((oEvent.getParameter("listItems") || [])[0]);
        }
        oListItem = oListItem || oSelectedItems[0] || null;
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
