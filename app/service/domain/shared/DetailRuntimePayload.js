sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts"
], function (UseCaseValue, CreateSentinel, ModelContracts, ModelPathContracts) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var DETAIL_MODEL = ModelContracts.MODELS.DETAIL;

    function sanitizeId(vId) {
        return String(vId || "").trim();
    }

    function isRealId(vId) {
        var sId = sanitizeId(vId);
        return !!sId && !CreateSentinel.isCreateId(sId);
    }

    function resolveCanonicalDbKey(mInput, mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        var sInputDbKey = sanitizeId((mInput && (mInput.dbKey || mInput.DB_KEY)) || UseCaseValue.rootId(mInput));
        var sSelectedRootId = sanitizeId(oUiState && oUiState.get(STATE_MODEL, "/postOpenHydratedRootId"));
        var sSelectedSnapshotRootId = sanitizeId(oUiState && oUiState.get(DETAIL_MODEL, "/current/root/id"));
        var sActiveRootId = sanitizeId(oUiState && oUiState.get(STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID));
        var sSelectedId = sanitizeId(oUiState && oUiState.get(STATE_MODEL, ModelPathContracts.SELECTED_ID));
        var aCandidates = [sInputDbKey, sSelectedSnapshotRootId, sSelectedRootId, sActiveRootId, sSelectedId];
        var i;
        for (i = 0; i < aCandidates.length; i += 1) {
            if (isRealId(aCandidates[i])) {
                return aCandidates[i];
            }
        }
        for (i = 0; i < aCandidates.length; i += 1) {
            if (aCandidates[i]) {
                return aCandidates[i];
            }
        }
        return "";
    }

    function dbKey(mInput, mCtx) {
        return resolveCanonicalDbKey(mInput, mCtx);
    }

    function rootId(mInput, mCtx) {
        return dbKey(mInput, mCtx);
    }

    function sessionGuid(mInput, mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return String(
            (mInput && (mInput.sessionGuid || mInput.SessionGuid))
            || (oUiState && oUiState.get(STATE_MODEL, ModelPathContracts.SESSION_ID))
            || ""
        ).trim();
    }

    function tabSessionId(mInput, mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return String(
            (mInput && (mInput.tabSessionId || mInput.TabSessionId))
            || (oUiState && oUiState.get(STATE_MODEL, "/tabSessionId"))
            || ""
        ).trim();
    }

    function normalizeRootKey(sRootId) {
        return CreateSentinel.isCreateId(sRootId) ? "" : String(sRootId || "").trim();
    }

    function saveRequest(mInput) {
        return {
            dbKey: String((mInput && (mInput.dbKey || mInput.DB_KEY)) || UseCaseValue.rootId(mInput) || "").trim(),
            sessionGuid: String((mInput && mInput.sessionGuid) || "").trim(),
            delta: (mInput && mInput.delta) || {},
            attachments: (mInput && mInput.attachments) || []
        };
    }

    function lockRequest(mInput, mCtx) {
        var sRootId = dbKey(mInput, mCtx);
        return {
            dbKey: sRootId,
            rootId: sRootId,
            sessionGuid: sessionGuid(mInput, mCtx),
            tabSessionId: tabSessionId(mInput, mCtx)
        };
    }

    return {
        resolveCanonicalDbKey: resolveCanonicalDbKey,
        dbKey: dbKey,
        rootId: rootId,
        sessionGuid: sessionGuid,
        tabSessionId: tabSessionId,
        normalizeRootKey: normalizeRootKey,
        saveRequest: saveRequest,
        lockRequest: lockRequest
    };
});
