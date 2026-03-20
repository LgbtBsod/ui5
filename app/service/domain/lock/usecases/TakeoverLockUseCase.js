sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants"
], function (Result, Effects, ModelContracts, StatePaths, ModelPathContracts, WorkflowContracts) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;

    function TakeoverLockUseCase() {
        return {
            execute: execute
        };
    }

function execute(mInput, mCtx) {
        var oLock = mCtx && mCtx.lock;
        var oUiState = mCtx && mCtx.uiState;
        var sRootId = (mInput && mInput.rootId) || (oUiState && oUiState.get(STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID));
        var sSessionGuid = (oUiState && oUiState.get(STATE_MODEL, StatePaths.SESSION_ID)) || "";

        if (!sRootId || !sSessionGuid || !oLock || typeof oLock.acquire !== "function") {
            return Promise.resolve(Result.fail({ code: "TAKEOVER_UNAVAILABLE" }));
        }

        return Promise.resolve(oLock.acquire({ rootId: sRootId, sessionGuid: sSessionGuid, force: true })).then(function (oRes) {
            if (!(oRes && oRes.ok)) {
                return Result.fail({ code: "TAKEOVER_FAILED", lock: oRes || {} });
            }
            return Result.ok({ ok: true, lock: oRes }, [
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.EDIT),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.EDIT_LOCKED),
                Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, true),
            ]);
        });
    }

    return TakeoverLockUseCase;
});