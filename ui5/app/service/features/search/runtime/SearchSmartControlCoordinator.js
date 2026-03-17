sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ChecklistIdentity",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchBindingPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/RebindDebouncePolicy"
], function (ChecklistIdentity, SearchBindingPolicy, RebindDebouncePolicy) {
    "use strict";

    function extractChecklistIdsFromListItems(aListItems) {
        return normalizeChecklistIds((aListItems || []).map(function (oListItem) {
            var oCtx = oListItem && oListItem.getBindingContext && oListItem.getBindingContext();
            var oObject = oCtx && oCtx.getObject && oCtx.getObject();
            return ChecklistIdentity.extractChecklistId(oObject);
        }));
    }

    function extractChecklistIdsFromContexts(aContexts) {
        return normalizeChecklistIds((aContexts || []).map(function (oCtx) {
            var oObject = oCtx && oCtx.getObject && oCtx.getObject();
            return ChecklistIdentity.extractChecklistId(oObject);
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
        return ChecklistIdentity.normalizeChecklistIds(
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
        extractChecklistId: ChecklistIdentity.extractChecklistId,
        extractChecklistIdFromSelectionEvent: extractChecklistIdFromSelectionEvent,
        extractChecklistIdsFromListItems: extractChecklistIdsFromListItems,
        extractObjectsFromContexts: extractObjectsFromContexts,
        extractChecklistIdsFromSelectionEvent: extractChecklistIdsFromSelectionEvent
    }, ChecklistIdentity, SearchBindingPolicy, RebindDebouncePolicy);
});
