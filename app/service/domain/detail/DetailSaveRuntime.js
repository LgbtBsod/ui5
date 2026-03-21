sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/ChecklistValidationService",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailUseCaseConstants"
], function (WorkflowContracts, ChecklistValidationService, ModelContracts, DetailUseCaseConstants) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var DETAIL_MODEL_PATHS = DetailUseCaseConstants.MODEL_PATHS;

    function isFilled(vValue) {
        return String(vValue == null ? "" : vValue).trim().length > 0;
    }

    function preserveBasicFields(oSavedSnapshot, oCurrentSnapshot, oBaseSnapshot) {
        var oSaved = oSavedSnapshot || {};
        var oSavedBasic = Object.assign({}, oSaved.basic || {});
        var oCurrentBasic = (oCurrentSnapshot && oCurrentSnapshot.basic) || {};
        var oBaseBasic = (oBaseSnapshot && oBaseSnapshot.basic) || {};
        var mMergedFields = Object.assign({}, oBaseBasic, oCurrentBasic);

        Object.keys(mMergedFields).forEach(function (sField) {
            if (isFilled(oSavedBasic[sField])) {
                return;
            }
            if (isFilled(oCurrentBasic[sField])) {
                oSavedBasic[sField] = oCurrentBasic[sField];
                return;
            }
            if (isFilled(oBaseBasic[sField])) {
                oSavedBasic[sField] = oBaseBasic[sField];
            }
        });

        return Object.assign({}, oSaved, { basic: oSavedBasic });
    }

    function normalizeOverallResult(oSnapshot) {
        var oCurrent = oSnapshot || {};
        var oRoot = Object.assign({}, oCurrent.root || {});
        var vOverall = ChecklistValidationService.buildOverallResult(oCurrent);

        oRoot.overall_result = vOverall;
        oRoot.OverallResult = vOverall;

        return Object.assign({}, oCurrent, { root: oRoot });
    }

    function readCurrentChecklist(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return (oUiState && oUiState.get(MODELS.DETAIL, DETAIL_MODEL_PATHS.ROOT)) || {};
    }

    function readBaseSnapshot(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return (oUiState && oUiState.get(MODELS.DETAIL, DETAIL_MODEL_PATHS.BASE)) || {};
    }

    function resolveVersionNumber(oCurrent, oSnapshot) {
        var nVersion = Number(
            (oCurrent && oCurrent.root && (oCurrent.root.version_number || oCurrent.root.VersionNumber)) ||
            (oCurrent && oCurrent.meta && oCurrent.meta.versionNumber) ||
            (oSnapshot && oSnapshot.root && (oSnapshot.root.version_number || oSnapshot.root.VersionNumber)) ||
            (oSnapshot && oSnapshot.meta && oSnapshot.meta.versionNumber) ||
            0
        );
        return Number.isFinite(nVersion) ? nVersion : 0;
    }

    function readSessionGuid(mCtx, StatePaths) {
        var oUiState = mCtx && mCtx.uiState;
        return (oUiState && oUiState.get(MODELS.STATE, StatePaths.SESSION_ID)) || "";
    }

    function readLockState(mCtx, StatePaths) {
        var oUiState = mCtx && mCtx.uiState;
        return WorkflowContracts.normalizeLockState(oUiState && oUiState.get(MODELS.STATE, StatePaths.WORKFLOW_DETAIL_LOCK_STATE));
    }

    return {
        normalizeOverallResult: normalizeOverallResult,
        preserveBasicFields: preserveBasicFields,
        readBaseSnapshot: readBaseSnapshot,
        readCurrentChecklist: readCurrentChecklist,
        readLockState: readLockState,
        readSessionGuid: readSessionGuid,
        resolveVersionNumber: resolveVersionNumber
    };
});
