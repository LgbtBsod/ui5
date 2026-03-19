sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPersistenceRuntime"
], function (UseCase, Result, Effects, ModelStateRuntime, DetailRuntimePayload, StatePaths, CreateSentinel, ModelPathContracts, WorkflowContracts, DetailPersistenceRuntime) {
    "use strict";

    function ForceReadOnlyUseCase() {
        UseCase.call(this, "ForceReadOnlyUseCase");
    }

    ForceReadOnlyUseCase.prototype = Object.create(UseCase.prototype);
    ForceReadOnlyUseCase.prototype.constructor = ForceReadOnlyUseCase;

    function isLockLostReason(sReason) {
        var sNormalized = String(sReason || "").toUpperCase();
        return sNormalized === WorkflowContracts.REASONS.KILLED || sNormalized === WorkflowContracts.REASONS.EXPIRED || sNormalized === WorkflowContracts.REASONS.LOCK_EXPIRED || sNormalized === WorkflowContracts.REASONS.LOST;
    }

    function isIdleTimeoutReason(sReason) {
        return String(sReason || "").toUpperCase() === WorkflowContracts.REASONS.IDLE_TIMEOUT;
    }

    function resolvePersistenceState(sReason) {
        if (isLockLostReason(sReason)) {
            return DetailPersistenceRuntime.STATES.LOCK_LOST;
        }
        if (isIdleTimeoutReason(sReason)) {
            return DetailPersistenceRuntime.STATES.IDLE_TIMEOUT_GRACE;
        }
        return DetailPersistenceRuntime.STATES.READ_ONLY;
    }

    function resolvePersistenceMessageKey(sReason) {
        if (isLockLostReason(sReason)) {
            return "persistenceLockLost";
        }
        if (isIdleTimeoutReason(sReason)) {
            return "persistenceIdleTimeoutGrace";
        }
        return "persistenceForcedReadOnly";
    }

    function shouldRestoreSnapshotState(sReason, bPreserveDirty) {
        if (isLockLostReason(sReason)) {
            return true;
        }
        return !bPreserveDirty;
    }

    ForceReadOnlyUseCase.prototype.execute = function (mInput, mCtx) {
        var sReason = String((mInput && mInput.reason) || WorkflowContracts.REASONS.READ_ONLY).trim() || WorkflowContracts.REASONS.READ_ONLY;
        var sMessageKey = String((mInput && mInput.messageKey) || "").trim();
        var bPreserveDirty = !!(mInput && mInput.preserveDirty) && !isLockLostReason(sReason);
        var oUiState = mCtx && mCtx.uiState;
        var oLockPort = mCtx && mCtx.lock;
        var sRootId = DetailRuntimePayload.rootId(mInput, mCtx);
        var sSessionGuid = DetailRuntimePayload.sessionGuid(mInput, mCtx, StatePaths);
        var sMode = WorkflowContracts.normalizeEditMode(oUiState && oUiState.get("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE));
        var sLockState = WorkflowContracts.normalizeLockState(oUiState && oUiState.get("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE));
        var oStateModel = mCtx && mCtx.stateModel;
        var oSelectedState = (oUiState && typeof oUiState.get === "function" && oUiState.get("selected", "/")) || {};
        var oSnapshotState = (oUiState && typeof oUiState.get === "function" && oUiState.get("snapshot", "/")) || {};
        var sTransitionState = isIdleTimeoutReason(sReason) ? "IDLE_TIMEOUT_GRACE" : (isLockLostReason(sReason) ? "LOCK_LOST" : "FORCED_READ_ONLY");
        var bRestoreSnapshotState = shouldRestoreSnapshotState(sReason, bPreserveDirty);
        var bShouldRelease = !!(
            sRootId &&
            sSessionGuid &&
            !CreateSentinel.isCreateId(sRootId) &&
            WorkflowContracts.isEditLocked(sMode, sLockState) &&
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
            var sPersistenceState = resolvePersistenceState(sReason);
            var sPersistenceMessageKey = resolvePersistenceMessageKey(sReason);
            aEffects = [
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, sTransitionState),
                Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
                Effects.modelPatch("state", StatePaths.WORKFLOW_LOCK_LOST_REASON, sReason),
                Effects.modelPatch("state", StatePaths.PENDING_NAVIGATION_INTENT, null),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, bPreserveDirty),
                Effects.modelPatch("state", ModelPathContracts.LOCK_EXPIRES, null),
                Effects.modelPatch("uiState", "/lock", {
                    ok: false,
                    reason: sReason,
                    isKilled: String(sReason || "").toUpperCase() === WorkflowContracts.REASONS.KILLED
                })
            ].concat(DetailPersistenceRuntime.failureEffects("manual", {
                code: sReason,
                message: sReason
            }, {
                state: sPersistenceState,
                messageKey: sPersistenceMessageKey,
                hasValidLock: false,
                lockOwnerSessionMatches: false,
                isManualSaveInFlight: false,
                isAutoSaveInFlight: false,
                currentWriteRequestId: ""
            }).effects);

            if (bRestoreSnapshotState) {
                aEffects.push(Effects.modelPatch("selected", "/", oSnapshotState));
            } else if (bPreserveDirty) {
                aEffects.push(Effects.modelPatch("selected", "/", oSelectedState));
            }

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
