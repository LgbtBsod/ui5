sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DomainStatePaths"
], function (UseCase, Result, Effects, ModelStateRuntime, DetailRuntimePayload, StatePaths, CreateSentinel, DomainStatePaths) {
    "use strict";

    function ForceReadOnlyUseCase() {
        UseCase.call(this, "ForceReadOnlyUseCase");
    }

    ForceReadOnlyUseCase.prototype = Object.create(UseCase.prototype);
    ForceReadOnlyUseCase.prototype.constructor = ForceReadOnlyUseCase;

    function isLockLostReason(sReason) {
        var sNormalized = String(sReason || "").toUpperCase();
        return sNormalized === "KILLED" || sNormalized === "EXPIRED" || sNormalized === "LOCK_EXPIRED" || sNormalized === "LOST";
    }

    function isIdleTimeoutReason(sReason) {
        return String(sReason || "").toUpperCase() === "IDLE_TIMEOUT";
    }

    ForceReadOnlyUseCase.prototype.execute = function (mInput, mCtx) {
        var sReason = String((mInput && mInput.reason) || "READ_ONLY").trim() || "READ_ONLY";
        var sMessageKey = String((mInput && mInput.messageKey) || "").trim();
        var bPreserveDirty = !!(mInput && mInput.preserveDirty);
        var oUiState = mCtx && mCtx.uiState;
        var oLockPort = mCtx && mCtx.lock;
        var sRootId = DetailRuntimePayload.rootId(mInput, mCtx);
        var sSessionGuid = DetailRuntimePayload.sessionGuid(mInput, mCtx, StatePaths);
        var sMode = String((oUiState && oUiState.get("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE)) || "").toUpperCase();
        var sLockState = String((oUiState && oUiState.get("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE)) || "").toUpperCase();
        var oStateModel = mCtx && mCtx.stateModel;
        var sTransitionState = isIdleTimeoutReason(sReason) ? "IDLE_TIMEOUT_GRACE" : (isLockLostReason(sReason) ? "LOCK_LOST" : "FORCED_READ_ONLY");
        var bShouldRelease = !!(
            sRootId &&
            sSessionGuid &&
            !CreateSentinel.isCreateId(sRootId) &&
            sMode === "EDIT" &&
            sLockState === "EDIT_LOCKED" &&
            oLockPort &&
            typeof oLockPort.release === "function"
        );
        var aEffects;
        if (oStateModel) {
            ModelStateRuntime.writeOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, sTransitionState);
        }
        var pRelease = bShouldRelease
            ? Promise.resolve(oLockPort.release(DetailRuntimePayload.lockRequest(mInput, mCtx, StatePaths))).catch(function () {
                return { ok: false, released: false, messageKey: "lockReleaseFailed" };
            })
            : Promise.resolve(null);

        return pRelease.then(function (oReleaseResult) {
            aEffects = [
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "READ"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, sTransitionState),
                Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "IDLE"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_LOCK_LOST_REASON, sReason),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, bPreserveDirty),
                Effects.modelPatch("state", DomainStatePaths.LOCK_EXPIRES, null),
                Effects.modelPatch("uiState", "/lock", {
                    ok: false,
                    reason: sReason,
                    isKilled: String(sReason || "").toUpperCase() === "KILLED"
                })
            ];

            if (sMessageKey) {
                aEffects.push(Effects.warn(sMessageKey));
            }
            if (bShouldRelease && !isLockLostReason(sReason) && (!oReleaseResult || oReleaseResult.ok === false || oReleaseResult.released === false)) {
                aEffects.push(Effects.warn((oReleaseResult && oReleaseResult.messageKey) || "lockReleaseFailed"));
            }

            return Result.ok({ forced: true, reason: sReason, release: oReleaseResult || null }, aEffects);
        });
    };

    return ForceReadOnlyUseCase;
});
