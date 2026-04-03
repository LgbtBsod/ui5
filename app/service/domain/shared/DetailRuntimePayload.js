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
        var sInputDbKey = sanitizeId((mInput && (mInput.dbKey || mInput.DB_KEY)) || UseCaseValue.requestedDbKey(mInput));
        var sHydratedDbKey = sanitizeId(oUiState && oUiState.get(STATE_MODEL, "/postOpenHydratedRootId"));
        var sSnapshotDbKey = sanitizeId(oUiState && oUiState.get(DETAIL_MODEL, "/current/root/id"));
        var sActiveDbKey = sanitizeId(oUiState && oUiState.get(STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID));
        var sSelectedId = sanitizeId(oUiState && oUiState.get(STATE_MODEL, ModelPathContracts.SELECTED_ID));
        var aCandidates = [sInputDbKey, sSnapshotDbKey, sHydratedDbKey, sActiveDbKey, sSelectedId];
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

    function requestedDbKey(mInput, mCtx) {
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

    function normalizePersistedDbKey(sDbKey) {
        return CreateSentinel.isCreateId(sDbKey) ? "" : String(sDbKey || "").trim();
    }

    function saveRequest(mInput) {
        return {
            dbKey: String((mInput && (mInput.dbKey || mInput.DB_KEY)) || UseCaseValue.requestedDbKey(mInput) || "").trim(),
            sessionGuid: String((mInput && mInput.sessionGuid) || "").trim(),
            delta: (mInput && mInput.delta) || {},
            attachments: (mInput && mInput.attachments) || []
        };
    }

    function lockRequest(mInput, mCtx) {
        var sDbKey = dbKey(mInput, mCtx);
        return {
            dbKey: sDbKey,
            sessionGuid: sessionGuid(mInput, mCtx),
            tabSessionId: tabSessionId(mInput, mCtx)
        };
    }

    return {
        resolveCanonicalDbKey: resolveCanonicalDbKey,
        dbKey: dbKey,
        requestedDbKey: requestedDbKey,
        sessionGuid: sessionGuid,
        tabSessionId: tabSessionId,
        normalizePersistedDbKey: normalizePersistedDbKey,
        saveRequest: saveRequest,
        lockRequest: lockRequest
    };
});
