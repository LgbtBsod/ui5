sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseInputUtils",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel"
], function (UseCaseInputUtils, CreateSentinel) {
    "use strict";

    function rootId(mInput, mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return UseCaseInputUtils.rootId(mInput) || String((oUiState && oUiState.get("state", "/activeObjectId")) || "").trim();
    }

    function sessionGuid(mInput, mCtx, StatePaths) {
        var oUiState = mCtx && mCtx.uiState;
        return String(
            (mInput && (mInput.sessionGuid || mInput.SessionGuid))
            || (oUiState && oUiState.get("state", StatePaths && StatePaths.SESSION_ID || "/sessionId"))
            || ""
        ).trim();
    }

    function normalizeRootKey(sRootId) {
        return CreateSentinel.isCreateId(sRootId) ? "" : String(sRootId || "").trim();
    }

    function saveRequest(mInput) {
        return {
            rootId: UseCaseInputUtils.rootId(mInput),
            sessionGuid: String((mInput && mInput.sessionGuid) || "").trim(),
            delta: (mInput && mInput.delta) || {},
            attachments: (mInput && mInput.attachments) || []
        };
    }

    function lockRequest(mInput, mCtx, StatePaths) {
        return {
            rootId: rootId(mInput, mCtx),
            sessionGuid: sessionGuid(mInput, mCtx, StatePaths)
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
