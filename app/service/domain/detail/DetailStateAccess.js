sap.ui.define([], function () {
    "use strict";

    function uiState(mCtx) {
        return mCtx && mCtx.uiState;
    }

    function readCurrentChecklist(mCtx) {
        var oUiState = uiState(mCtx);
        return (oUiState && oUiState.get("selected", "/")) || {};
    }

    function readCurrentAttachments(mCtx) {
        var oChecklist = readCurrentChecklist(mCtx);
        return (oChecklist && oChecklist.attachments) || [];
    }

    function readDetailSnapshot(mCtx) {
        var oUiState = uiState(mCtx);
        return (oUiState && oUiState.get("snapshot", "/")) || {};
    }

    function readRequiredFields(mCtx) {
        var oUiState = uiState(mCtx);
        var aRequired = oUiState && oUiState.get("state", "/requiredFields");
        return Array.isArray(aRequired) && aRequired.length ? aRequired : null;
    }

    function resolveDateCheck(mCtx) {
        var oBasic = (readCurrentChecklist(mCtx) || {}).basic || {};
        var sDate = String(oBasic.date || oBasic.DateCheck || "").trim();
        return /^\d{4}-\d{2}-\d{2}$/.test(sDate) ? sDate : "";
    }

    return {
        readCurrentAttachments: readCurrentAttachments,
        readCurrentChecklist: readCurrentChecklist,
        readDetailSnapshot: readDetailSnapshot,
        readRequiredFields: readRequiredFields,
        resolveDateCheck: resolveDateCheck
    };
});
