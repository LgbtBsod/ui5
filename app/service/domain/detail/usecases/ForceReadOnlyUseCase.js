sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPersistenceRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants"
], function (Result, Effects, ModelStateRuntime, DetailRuntimePayload, StatePaths, CreateSentinel, ModelPathContracts, WorkflowContracts, DetailPersistenceRuntime, ModelContracts, DetailContracts, MessageKeyConstants) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var DETAIL_MODEL = MODELS.DETAIL;
    var STATE_MODEL = MODELS.STATE;
    var DETAIL_MESSAGE_KEYS = MessageKeyConstants.VIEW;
    var DETAIL_MODEL_PATHS = DetailContracts.MODEL_PATHS;

    function ForceReadOnlyUseCase() {
        return {
            execute: function (input, ctx) { return execute(input, ctx); }
        };
    }

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
            return DETAIL_MESSAGE_KEYS.PERSISTENCE_LOCK_LOST;
        }
        if (isIdleTimeoutReason(sReason)) {
            return DETAIL_MESSAGE_KEYS.PERSISTENCE_IDLE_TIMEOUT_GRACE;
        }
        return DETAIL_MESSAGE_KEYS.PERSISTENCE_FORCED_READ_ONLY;
    }

    function shouldRestoreSnapshotState(sReason, bPreserveDirty) {
        if (isLockLostReason(sReason)) {
            return true;
        }
        return !bPreserveDirty;
    }

    function execute(mInput, mCtx) {
        var sReason = String((mInput && mInput.reason) || WorkflowContracts.REASONS.READ_ONLY).trim() || WorkflowContracts.REASONS.READ_ONLY;
        var sMessageKey = String((mInput && mInput.messageKey) || "").trim();
        var bPreserveDirty = !!(mInput && mInput.preserveDirty) && !isLockLostReason(sReason);
        var oUiState = mCtx && mCtx.uiState;
        var oLockPort = mCtx && mCtx.lock;
        var sDbKey = DetailRuntimePayload.dbKey(mInput, mCtx);
        var sSessionGuid = DetailRuntimePayload.sessionGuid(mInput, mCtx, StatePaths);
        var sMode = WorkflowContracts.normalizeEditMode(oUiState && oUiState.get(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE));
        var sLockState = WorkflowContracts.normalizeLockState(oUiState && oUiState.get(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_LOCK_STATE));
        var oStateModel = mCtx && mCtx.stateModel;
        var oSelectedState = (oUiState && typeof oUiState.get === "function" && oUiState.get(DETAIL_MODEL, DETAIL_MODEL_PATHS.ROOT)) || {};
        var oSnapshotState = (oUiState && typeof oUiState.get === "function" && oUiState.get(DETAIL_MODEL, DETAIL_MODEL_PATHS.BASE)) || {};
        var sTransitionState = isIdleTimeoutReason(sReason)
            ? WorkflowContracts.LOCK_STATES.IDLE_TIMEOUT_GRACE
            : (isLockLostReason(sReason) ? WorkflowContracts.LOCK_STATES.LOCK_LOST : WorkflowContracts.LOCK_STATES.FORCED_READ_ONLY);
        var bRestoreSnapshotState = shouldRestoreSnapshotState(sReason, bPreserveDirty);
        var bShouldRelease = !!(
            sDbKey &&
            sSessionGuid &&
            !CreateSentinel.isCreateId(sDbKey) &&
            WorkflowContracts.isEditLocked(sMode, sLockState) &&
            oLockPort &&
            typeof oLockPort.release === "function"
        );
        var aEffects;
        if (oStateModel) {
            ModelStateRuntime.writeOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, sTransitionState);
        }
        var pRelease = bShouldRelease
            ? Promise.resolve(oLockPort.release(DetailRuntimePayload.lockRequest(Object.assign({}, mInput || {}, { dbKey: sDbKey }), mCtx, StatePaths))).catch(function () {
                return { ok: false, released: false, messageKey: DETAIL_MESSAGE_KEYS.LOCK_RELEASE_FAILED };
            })
            : Promise.resolve(null);

        return pRelease.then(function (oReleaseResult) {
            var sPersistenceState = resolvePersistenceState(sReason);
            var sPersistenceMessageKey = resolvePersistenceMessageKey(sReason);
            aEffects = [
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ),
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, sTransitionState),
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_LOCK_LOST_REASON, sReason),
                Effects.modelPatch(STATE_MODEL, StatePaths.PENDING_NAVIGATION_INTENT, null),
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DIRTY, bPreserveDirty),
                Effects.modelPatch(STATE_MODEL, ModelPathContracts.LOCK_EXPIRES, null),
                Effects.modelPatch(MODELS.SHELL, MODEL_PATHS.SHELL_LOCK, {
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
                aEffects.push(Effects.modelPatch(DETAIL_MODEL, DETAIL_MODEL_PATHS.ROOT, oSnapshotState));
            } else if (bPreserveDirty) {
                aEffects.push(Effects.modelPatch(DETAIL_MODEL, DETAIL_MODEL_PATHS.ROOT, oSelectedState));
            }

            if (sMessageKey) {
                aEffects.push(Effects.warn(sMessageKey));
            }
            if (bShouldRelease && !isLockLostReason(sReason) && (!oReleaseResult || oReleaseResult.ok === false || oReleaseResult.released === false)) {
                aEffects.push(Effects.warn((oReleaseResult && oReleaseResult.messageKey) || DETAIL_MESSAGE_KEYS.LOCK_RELEASE_FAILED));
            }

            return Result.ok({ forced: true, reason: sReason, release: oReleaseResult || null }, aEffects);
        });
    }

    return ForceReadOnlyUseCase;
});
