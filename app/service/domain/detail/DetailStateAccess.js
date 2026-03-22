sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts"
], function (ModelContracts, ModelPathContracts, DetailUseCaseConstants, ViewPathContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var DETAIL_MODEL = MODELS.DETAIL;
    var STATE_MODEL = MODELS.STATE;
    var VIEW_MODEL = MODELS.VIEW;
    var DETAIL_MODEL_PATHS = DetailUseCaseConstants.MODEL_PATHS;

    function uiState(mCtx) {
        return mCtx && mCtx.uiState;
    }

    function readCurrentChecklist(mCtx) {
        var oUiState = uiState(mCtx);
        return (oUiState && oUiState.get(DETAIL_MODEL, DETAIL_MODEL_PATHS.ROOT)) || {};
    }

    function readCurrentAttachments(mCtx) {
        var oChecklist = readCurrentChecklist(mCtx);
        return (oChecklist && oChecklist.attachments) || [];
    }

    function readSessionAttachments(mCtx) {
        var oUiState = uiState(mCtx);
        var aSession = oUiState && oUiState.get(VIEW_MODEL, ViewPathContracts.SESSION_ATTACHMENTS);
        return Array.isArray(aSession) ? aSession : [];
    }

    function readWorkingAttachments(mCtx) {
        return readCurrentAttachments(mCtx).concat(readSessionAttachments(mCtx));
    }

    function readDetailSnapshot(mCtx) {
        var oUiState = uiState(mCtx);
        var oSnapshot = (oUiState && oUiState.get(DETAIL_MODEL, DETAIL_MODEL_PATHS.BASE)) || {};
        var aBaseAttachments = oUiState && oUiState.get(DETAIL_MODEL, DETAIL_MODEL_PATHS.BASE_ATTACHMENTS);
        return Object.assign({}, oSnapshot, {
            attachments: Array.isArray(aBaseAttachments) ? aBaseAttachments : []
        });
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
        readSessionAttachments: readSessionAttachments,
        readWorkingAttachments: readWorkingAttachments,
        resolveDateCheck: resolveDateCheck
    };
});
