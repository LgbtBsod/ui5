sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (UseCase, Result, Effects, StatePaths) {
    "use strict";

    function TakeoverLockUseCase() {
        UseCase.call(this, "TakeoverLockUseCase");
    }

    TakeoverLockUseCase.prototype = Object.create(UseCase.prototype);
    TakeoverLockUseCase.prototype.constructor = TakeoverLockUseCase;

    TakeoverLockUseCase.prototype.execute = function (mInput, mCtx) {
        var oLock = mCtx && mCtx.lock;
        var oUiState = mCtx && mCtx.uiState;
        var sRootId = (mInput && mInput.rootId) || (oUiState && oUiState.get("state", "/activeObjectId"));
        var sSessionGuid = (oUiState && oUiState.get("state", StatePaths.SESSION_ID)) || "";

        if (!sRootId || !sSessionGuid || !oLock || typeof oLock.acquire !== "function") {
            return Promise.resolve(Result.fail({ code: "TAKEOVER_UNAVAILABLE" }));
        }

        return Promise.resolve(oLock.acquire({ rootId: sRootId, sessionGuid: sSessionGuid, force: true })).then(function (oRes) {
            if (!(oRes && oRes.ok)) {
                return Result.fail({ code: "TAKEOVER_FAILED", lock: oRes || {} });
            }
            return Result.ok({ ok: true, lock: oRes }, [
                Effects.modelPatch("state", StatePaths.WORKFLOW_EDIT_MODE, "EDIT"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_LOCK_STATUS, "LOCKED"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, true),
            ]);
        });
    };

    return TakeoverLockUseCase;
});
