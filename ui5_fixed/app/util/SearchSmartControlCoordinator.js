sap.ui.define([
    "checklist/app/util/search/SearchBindingPolicy",
    "checklist/app/util/search/RebindDebouncePolicy"
], function (SearchBindingPolicy, RebindDebouncePolicy) {
    "use strict";

    function extractChecklistId(oObject) {
        if (!oObject) { return ""; }
        return String(
            oObject.Key ||
            oObject.key ||
            oObject.Uuid ||
            oObject.id ||
            oObject.ID ||
            oObject.Id ||
            oObject.ChecklistId ||
            oObject.checklist_id ||
            oObject.CHECKLIST_ID ||
            (((oObject.root || {}).id) || "")
        ).trim();
    }

    function normalizeChecklistIds(aIds) {
        var aSeen = {};
        return (aIds || []).reduce(function (aAcc, sId) {
            var sNorm = String(sId || "").trim();
            if (!sNorm || aSeen[sNorm]) {
                return aAcc;
            }
            aSeen[sNorm] = true;
            aAcc.push(sNorm);
            return aAcc;
        }, []);
    }

    function extractChecklistIdsFromListItems(aListItems) {
        return normalizeChecklistIds((aListItems || []).map(function (oListItem) {
            var oCtx = oListItem && oListItem.getBindingContext && oListItem.getBindingContext();
            var oObject = oCtx && oCtx.getObject && oCtx.getObject();
            return extractChecklistId(oObject);
        }));
    }

    function extractChecklistIdsFromContexts(aContexts) {
        return normalizeChecklistIds((aContexts || []).map(function (oCtx) {
            var oObject = oCtx && oCtx.getObject && oCtx.getObject();
            return extractChecklistId(oObject);
        }));
    }

    function extractObjectsFromContexts(aContexts) {
        return (aContexts || []).map(function (oCtx) {
            return oCtx && oCtx.getObject ? (oCtx.getObject() || null) : null;
        }).filter(Boolean);
    }

    function extractChecklistIdsFromSelectionEvent(oEvent, oTable) {
        var aListItems = [];
        var aSelectedContexts = [];
        var aRowContexts = [];
        if (!oEvent || typeof oEvent.getParameter !== "function") {
            return [];
        }
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
        aRowContexts = [];
        if (oEvent.getParameter("rowContext")) {
            aRowContexts.push(oEvent.getParameter("rowContext"));
        }
        return normalizeChecklistIds(
            extractChecklistIdsFromListItems(aListItems)
                .concat(extractChecklistIdsFromContexts(aSelectedContexts))
                .concat(extractChecklistIdsFromContexts(aRowContexts))
                .concat(
                    extractChecklistIdsFromListItems(
                        oTable && oTable.getSelectedItems ? (oTable.getSelectedItems() || []) : []
                    )
                )
        );
    }

    function extractChecklistIdFromSelectionEvent(oEvent) {
        return extractChecklistIdsFromSelectionEvent(oEvent)[0] || "";
    }

    return Object.assign({
        extractChecklistId: extractChecklistId,
        extractChecklistIdFromSelectionEvent: extractChecklistIdFromSelectionEvent,
        extractChecklistIdsFromListItems: extractChecklistIdsFromListItems,
        extractObjectsFromContexts: extractObjectsFromContexts,
        extractChecklistIdsFromSelectionEvent: extractChecklistIdsFromSelectionEvent
    }, SearchBindingPolicy, RebindDebouncePolicy);
});
