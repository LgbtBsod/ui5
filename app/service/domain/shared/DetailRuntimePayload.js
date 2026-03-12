sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts"
], function (UseCaseValue, CreateSentinel, ModelContracts, ModelPathContracts) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;

    function rootId(mInput, mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return UseCaseValue.rootId(mInput) || String((oUiState && oUiState.get(STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID)) || "").trim();
    }

    function sessionGuid(mInput, mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return String(
            (mInput && (mInput.sessionGuid || mInput.SessionGuid))
            || (oUiState && oUiState.get(STATE_MODEL, ModelPathContracts.SESSION_ID))
            || ""
        ).trim();
    }

    function normalizeRootKey(sRootId) {
        return CreateSentinel.isCreateId(sRootId) ? "" : String(sRootId || "").trim();
    }

    function saveRequest(mInput) {
        return {
            rootId: UseCaseValue.rootId(mInput),
            sessionGuid: String((mInput && mInput.sessionGuid) || "").trim(),
            delta: (mInput && mInput.delta) || {},
            attachments: (mInput && mInput.attachments) || []
        };
    }

    function lockRequest(mInput, mCtx, StatePaths) {
        return {
            rootId: rootId(mInput, mCtx),
            sessionGuid: sessionGuid(mInput, mCtx)
        };
    }

    return {
        rootId: rootId,
        sessionGuid: sessionGuid,
        normalizeRootKey: normalizeRootKey,
        saveRequest: saveRequest,
        lockRequest: lockRequest
    };
});
