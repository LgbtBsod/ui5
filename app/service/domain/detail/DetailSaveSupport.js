sap.ui.define([], function () {
    "use strict";

    function isFilled(vValue) {
        return String(vValue == null ? "" : vValue).trim().length > 0;
    }

    function preserveBasicFields(oSavedSnapshot, oCurrentSnapshot, oBaseSnapshot) {
        var oSaved = oSavedSnapshot || {};
        var oSavedBasic = Object.assign({}, oSaved.basic || {});
        var oCurrentBasic = (oCurrentSnapshot && oCurrentSnapshot.basic) || {};
        var oBaseBasic = (oBaseSnapshot && oBaseSnapshot.basic) || {};
        var aFields = ["date", "time", "timezone"];

        aFields.forEach(function (sField) {
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

    function readCurrentChecklist(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return (oUiState && oUiState.get("selected", "/")) || (oUiState && oUiState.get("uiState", "/_detailCurrent")) || {};
    }

    function readBaseSnapshot(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return (oUiState && oUiState.get("uiState", "/_detailSnapshot")) || {};
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
        return (oUiState && oUiState.get("state", StatePaths.SESSION_ID)) || "";
    }

    function readLockState(mCtx, StatePaths) {
        var oUiState = mCtx && mCtx.uiState;
        return String((oUiState && oUiState.get("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE)) || "").toUpperCase();
    }

    return {
        preserveBasicFields: preserveBasicFields,
        readBaseSnapshot: readBaseSnapshot,
        readCurrentChecklist: readCurrentChecklist,
        readLockState: readLockState,
        readSessionGuid: readSessionGuid,
        resolveVersionNumber: resolveVersionNumber
    };
});
