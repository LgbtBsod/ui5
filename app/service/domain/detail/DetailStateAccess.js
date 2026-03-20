sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts"
], function (ModelContracts, ModelPathContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var SELECTED_MODEL = MODELS.SELECTED;
    var SNAPSHOT_MODEL = "snapshot";
    var STATE_MODEL = MODELS.STATE;

    function uiState(mCtx) {
        return mCtx && mCtx.uiState;
    }

    function readCurrentChecklist(mCtx) {
        var oUiState = uiState(mCtx);
        return (oUiState && oUiState.get(SELECTED_MODEL, "/")) || {};
    }

    function readCurrentAttachments(mCtx) {
        var oChecklist = readCurrentChecklist(mCtx);
        return (oChecklist && oChecklist.attachments) || [];
    }

    function readWorkingAttachments(mCtx) {
        var oUiState = uiState(mCtx);
        var aSession = oUiState && oUiState.get("view", "/sessionAttachments");
        if (Array.isArray(aSession)) {
            return aSession;
        }
        return readCurrentAttachments(mCtx);
    }

    function readDetailSnapshot(mCtx) {
        var oUiState = uiState(mCtx);
        return (oUiState && oUiState.get(SNAPSHOT_MODEL, "/")) || {};
    }

    function readRequiredFields(mCtx) {
        var oUiState = uiState(mCtx);
        var aRequired = oUiState && oUiState.get(STATE_MODEL, ModelPathContracts.REQUIRED_FIELDS);
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
        readWorkingAttachments: readWorkingAttachments,
        resolveDateCheck: resolveDateCheck
    };
});
