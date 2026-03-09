sap.ui.define([
    "checklist/app/service/framework/UseCase",
    "checklist/app/service/framework/Result",
    "checklist/app/service/framework/Effects",
    "checklist/app/model/StatePaths",
    "checklist/app/util/CreateSentinel"
], function (UseCase, Result, Effects, StatePaths, CreateSentinel) {
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

    ForceReadOnlyUseCase.prototype.execute = function (mInput, mCtx) {
        var sReason = String((mInput && mInput.reason) || "READ_ONLY").trim() || "READ_ONLY";
        var sMessageKey = String((mInput && mInput.messageKey) || "").trim();
        var bPreserveDirty = !!(mInput && mInput.preserveDirty);
        var oUiState = mCtx && mCtx.uiState;
        var oLockPort = mCtx && mCtx.lock;
        var sRootId = String((mInput && mInput.rootId) || (oUiState && oUiState.get("state", "/activeObjectId")) || "").trim();
        var sSessionGuid = String((oUiState && oUiState.get("state", StatePaths.SESSION_ID)) || "").trim();
        var sMode = String((oUiState && oUiState.get("state", StatePaths.WORKFLOW_EDIT_MODE)) || "").toUpperCase();
        var sLockState = String((oUiState && oUiState.get("state", StatePaths.WORKFLOW_LOCK_STATUS)) || "").toUpperCase();
        var bShouldRelease = !!(
            sRootId &&
            sSessionGuid &&
            !CreateSentinel.isCreateId(sRootId) &&
            sMode === "EDIT" &&
            sLockState === "LOCKED" &&
            oLockPort &&
            typeof oLockPort.release === "function"
        );
        var aEffects;
        var pRelease = bShouldRelease
            ? Promise.resolve(oLockPort.release({ rootId: sRootId, sessionGuid: sSessionGuid })).catch(function () {
                return { ok: false, released: false, messageKey: "lockReleaseFailed" };
            })
            : Promise.resolve(null);

        return pRelease.then(function (oReleaseResult) {
            aEffects = [
                Effects.modelPatch("state", StatePaths.WORKFLOW_EDIT_MODE, "READ"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_LOCK_STATUS, "IDLE"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "IDLE"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_LOCK_LOST_REASON, sReason),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, bPreserveDirty),
                Effects.modelPatch("state", "/lockExpires", null),
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
