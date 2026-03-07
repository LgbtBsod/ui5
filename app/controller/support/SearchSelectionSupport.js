sap.ui.define([], function () {
    "use strict";

    function extractChecklistIdFromObject(oObject) {
        return String(
            (oObject && (oObject.Key || oObject.key || oObject.Id || oObject.id || oObject.RequestId || oObject.checklist_id)) || ""
        ).trim();
    }

    function normalizeChecklistIds(aIds) {
        var mSeen = {};
        return (aIds || []).reduce(function (aAcc, sId) {
            var sNorm = String(sId || "").trim();
            if (!sNorm || mSeen[sNorm]) {
                return aAcc;
            }
            mSeen[sNorm] = true;
            aAcc.push(sNorm);
            return aAcc;
        }, []);
    }

    function extractChecklistIdFromListItem(oListItem) {
        var oCtx = oListItem && oListItem.getBindingContext && oListItem.getBindingContext();
        var oObject = oCtx && oCtx.getObject && oCtx.getObject();
        return extractChecklistIdFromObject(oObject);
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
            return extractChecklistIdFromObject(oCtx && oCtx.getObject && oCtx.getObject());
        }));
        aIds = aIds.concat((aRowContexts || []).map(function (oCtx) {
            return extractChecklistIdFromObject(oCtx && oCtx.getObject && oCtx.getObject());
        }));
        aIds = aIds.concat(
            ((oTable && oTable.getSelectedItems && oTable.getSelectedItems()) || []).map(extractChecklistIdFromListItem)
        );
        return normalizeChecklistIds(aIds);
    }

    function extractSelectedRowId(oEvent) {
        return extractSelectedRowIds(oEvent)[0] || "";
    }

    return {
        extractChecklistIdFromObject: extractChecklistIdFromObject,
        extractSelectedRowIds: extractSelectedRowIds,
        extractSelectedRowId: extractSelectedRowId
    };
});
