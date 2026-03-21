sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ChecklistIdentity",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchBindingPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/RebindDebouncePolicy"
], function (ChecklistIdentity, SearchBindingPolicy, RebindDebouncePolicy) {
    "use strict";

    function extractChecklistIdsFromListItems(aListItems) {
        return ChecklistIdentity.normalizeChecklistIds((aListItems || []).map(function (oListItem) {
            var oCtx = oListItem && oListItem.getBindingContext && oListItem.getBindingContext();
            var oObject = oCtx && oCtx.getObject && oCtx.getObject();
            return ChecklistIdentity.extractChecklistId(oObject);
        }));
    }

    function extractChecklistIdsFromContexts(aContexts) {
        return ChecklistIdentity.normalizeChecklistIds((aContexts || []).map(function (oCtx) {
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
        var aSelectedContexts = [];
        var aRowContexts = [];
        var aSelectedItems = oTable && oTable.getSelectedItems ? (oTable.getSelectedItems() || []) : [];
        if (!oEvent || typeof oEvent.getParameter !== "function") {
            return [];
        }
        aSelectedContexts = oEvent.getParameter("selectedContexts") || [];
        aRowContexts = [];
        if (oEvent.getParameter("rowContext")) {
            aRowContexts.push(oEvent.getParameter("rowContext"));
        }
        return ChecklistIdentity.normalizeChecklistIds(
            extractChecklistIdsFromContexts(aSelectedContexts)
                .concat(extractChecklistIdsFromContexts(aRowContexts))
                .concat(extractChecklistIdsFromListItems(aSelectedItems))
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
