sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts"
], function (Result, Effects, ModelContracts, StatePaths, ModelPathContracts, WorkflowContracts) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var SHELL_MODEL = ModelContracts.MODELS.SHELL;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;

    function TakeoverLockUseCase() {
        return {
            execute: execute
        };
    }

function execute(mInput, mCtx) {
        var oLock = mCtx && mCtx.lock;
        var oUiState = mCtx && mCtx.uiState;
        var sDbKey = (mInput && (mInput.dbKey || mInput.rootId)) || (oUiState && oUiState.get(STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID));
        var sSessionGuid = (oUiState && oUiState.get(STATE_MODEL, StatePaths.SESSION_ID)) || "";

        if (!sDbKey || !sSessionGuid || !oLock || typeof oLock.acquire !== "function") {
            return Promise.resolve(Result.fail({ code: "TAKEOVER_UNAVAILABLE" }));
        }

        return Promise.resolve(oLock.acquire({
            dbKey: sDbKey,
            sessionGuid: sSessionGuid,
            forceTakeover: true
        })).then(function (oRes) {
            if (!(oRes && oRes.ok)) {
                return Result.fail({ code: "TAKEOVER_FAILED", lock: oRes || {} });
            }
            return Result.ok({ ok: true, lock: oRes }, [
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.EDIT),
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.EDIT_LOCKED),
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_AUTOSAVE_ENABLED, true),
                Effects.modelPatch(STATE_MODEL, StatePaths.PERSISTENCE_HAS_VALID_LOCK, true),
                Effects.modelPatch(STATE_MODEL, StatePaths.PERSISTENCE_LOCK_OWNER_SESSION_MATCHES, true),
                Effects.modelPatch(STATE_MODEL, StatePaths.PERSISTENCE_LAST_LOCK_REFRESH_AT, new Date().toISOString()),
                Effects.modelPatch(SHELL_MODEL, MODEL_PATHS.SHELL_LOCK, {
                    ok: true,
                    reason: WorkflowContracts.REASONS.OWNED_BY_YOU,
                    isKilled: false
                }),
            ]);
        });
    }

    return TakeoverLockUseCase;
});
