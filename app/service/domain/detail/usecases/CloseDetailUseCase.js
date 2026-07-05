sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowTelemetry",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants"
], function (Result, Effects, DetailRuntimePayload, StatePaths, CreateSentinel, WorkflowTelemetry, NavigationContracts, WorkflowContracts, ModelContracts, MessageKeyConstants) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var STATE_MODEL = MODELS.STATE;
    var SHELL_MODEL = MODELS.SHELL;
    var DETAIL_MESSAGE_KEYS = MessageKeyConstants.DETAIL;

    function resolveDbKey(mInput, mCtx, oUiState) {
        var sCanonical = DetailRuntimePayload.dbKey(mInput, mCtx);
        if (sCanonical || !oUiState || typeof oUiState.get !== "function") {
            return sCanonical || "";
        }
        return oUiState.get(STATE_MODEL, StatePaths.ACTIVE_OBJECT_ID) || oUiState.get(STATE_MODEL, StatePaths.SELECTED_ID) || "";
    }

    function isEditableLifecycle(oUiState) {
        var sEditMode = WorkflowContracts.normalizeEditMode(oUiState && typeof oUiState.get === "function" ? oUiState.get(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE) : "");
        var sLockState = WorkflowContracts.normalizeLockState(oUiState && typeof oUiState.get === "function" ? oUiState.get(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_LOCK_STATE) : "");
        return WorkflowContracts.isEditableMode(sEditMode)
            || sLockState === WorkflowContracts.LOCK_STATES.EDIT_LOCKED
            || sLockState === WorkflowContracts.LOCK_STATES.ACQUIRING_LOCK
            || sLockState === WorkflowContracts.LOCK_STATES.IDLE_TIMEOUT_GRACE;
    }

    function buildCloseEffects(sDbKey) {
        return [
            Effects.modelPatch(STATE_MODEL, StatePaths.READINESS_DETAIL, {
                status: WorkflowContracts.READINESS_STATUS.IDLE,
                ready: false,
                readyAt: "",
                error: "",
                dbKey: "",
                rootId: "",
                mode: WorkflowContracts.EDIT_MODES.READ,
                permissionKnown: false,
                lockKnown: false
            }),
            Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ),
            Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.IDLE),
            Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.IDLE),
            Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null),
            Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
            Effects.modelPatch(STATE_MODEL, StatePaths.SAVE_IN_FLIGHT, false),
            Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DIRTY, false),
            Effects.modelPatch(STATE_MODEL, StatePaths.PERSISTENCE_STATE, WorkflowContracts.PERSISTENCE_STATES.IDLE),
            Effects.modelPatch(STATE_MODEL, StatePaths.PERSISTENCE_NEXT_HEARTBEAT_AT, null),
            Effects.modelPatch(STATE_MODEL, StatePaths.PERSISTENCE_HAS_VALID_LOCK, false),
            Effects.modelPatch(STATE_MODEL, StatePaths.PERSISTENCE_LOCK_OWNER_SESSION_MATCHES, false),
            Effects.modelPatch(STATE_MODEL, StatePaths.LOCK_OPERATION_PENDING, false),
            Effects.modelPatch(SHELL_MODEL, MODEL_PATHS.SHELL_LAYOUT, NavigationContracts.LAYOUTS.ONE_COLUMN),
            Effects.modelPatch(STATE_MODEL, StatePaths.ACTIVE_OBJECT_ID, ""),
            Effects.modelPatch(STATE_MODEL, StatePaths.SELECTED_ID, ""),
            Effects.modelPatch(STATE_MODEL, StatePaths.POST_OPEN_HYDRATED_ROOT_ID, ""),
            Effects.modelPatch(STATE_MODEL, StatePaths.CURRENT_ROUTE_NAME, NavigationContracts.ROUTES.SEARCH),
            Effects.navigate(NavigationContracts.ROUTES.SEARCH, {}, true)
        ];
    }

    function isReleaseFailure(oReleaseResult) {
        return !oReleaseResult || oReleaseResult.ok === false || oReleaseResult.released === false;
    }

    function execute(mInput, mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        var oLockPort = mCtx && mCtx.lock;
        var sDbKey = resolveDbKey(mInput, mCtx, oUiState);
        var sSessionGuid = DetailRuntimePayload.sessionGuid(mInput, mCtx);
        var bShouldRelease = !!(
            sDbKey
            && !CreateSentinel.isCreateId(sDbKey)
            && sSessionGuid
            && oLockPort
            && typeof oLockPort.release === "function"
            && isEditableLifecycle(oUiState)
        );
        var pRelease = bShouldRelease
            ? Promise.resolve(oLockPort.release(DetailRuntimePayload.lockRequest(mInput, mCtx))).catch(function () {
                return { ok: false, code: "ERROR", released: false, messageKey: DETAIL_MESSAGE_KEYS.LOCK_RELEASE_FAILED };
            })
            : Promise.resolve();

        return pRelease.then(function (oReleaseResult) {
            var aEffects = buildCloseEffects(sDbKey);

            if (!bShouldRelease) {
                return Result.ok({ reason: (mInput && mInput.intent) || "close" }, aEffects);
            }

            WorkflowTelemetry.emit(isReleaseFailure(oReleaseResult) ? "lock.release.failed" : "lock.release.completed", {
                stateModel: mCtx && mCtx.stateModel,
                payload: { dbKey: sDbKey, source: "close_detail" }
            });

            if (isReleaseFailure(oReleaseResult)) {
                aEffects.push(Effects.warn((oReleaseResult && oReleaseResult.messageKey) || DETAIL_MESSAGE_KEYS.LOCK_RELEASE_FAILED));
            }
            return Result.ok({ reason: (mInput && mInput.intent) || "close" }, aEffects);
        });
    }

    function CloseDetailUseCase() {
        return {
            execute: execute
        };
    }

    return CloseDetailUseCase;
});
