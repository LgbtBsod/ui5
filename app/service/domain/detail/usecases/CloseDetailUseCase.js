sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel"
], function (UseCase, Result, Effects, DetailRuntimePayload, StatePaths, CreateSentinel) {
    "use strict";

    function CloseDetailUseCase() {
        UseCase.call(this, "CloseDetailUseCase");
    }

    CloseDetailUseCase.prototype = Object.create(UseCase.prototype);
    CloseDetailUseCase.prototype.constructor = CloseDetailUseCase;

    CloseDetailUseCase.prototype.execute = function (mInput, mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        var oLockPort = mCtx && mCtx.lock;
        var sRootId = DetailRuntimePayload.rootId(mInput, mCtx);
        var sSessionGuid = DetailRuntimePayload.sessionGuid(mInput, mCtx, StatePaths);
        var aEffects;

        var pRelease = Promise.resolve();
        if (sRootId && !CreateSentinel.isCreateId(sRootId) && sSessionGuid && oLockPort && typeof oLockPort.release === "function") {
            pRelease = Promise.resolve(oLockPort.release(DetailRuntimePayload.lockRequest(mInput, mCtx, StatePaths))).catch(function () {
                return { ok: false, code: "ERROR", released: false, messageKey: "lockReleaseFailed" };
            });
        }

        return pRelease.then(function (oReleaseResult) {
            aEffects = [
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "READ"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "IDLE"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "IDLE"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null),
                Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
                Effects.modelPatch("state", "/lockOperationPending", false),
                Effects.modelPatch("state", "/layout", "OneColumn"),
                Effects.modelPatch("state", "/activeObjectId", null),
                Effects.modelPatch("state", "/selectedId", null),
                Effects.navigate("search", {}, true)
            ];
            if (sRootId && !CreateSentinel.isCreateId(sRootId) && sSessionGuid && (!oReleaseResult || oReleaseResult.ok === false || oReleaseResult.released === false)) {
                aEffects.push(Effects.warn((oReleaseResult && oReleaseResult.messageKey) || "lockReleaseFailed"));
            }
            return Result.ok({ reason: (mInput && mInput.intent) || "close" }, aEffects);
        });
    };

    return CloseDetailUseCase;
});
