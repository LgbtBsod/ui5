sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
"PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowTelemetry",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants"
], function (Result, Effects, DetailRuntimePayload, StatePaths, CreateSentinel, WorkflowTelemetry, ModelPathContracts, NavigationContracts, WorkflowContracts, ModelContracts, DetailContracts, MessageKeyConstants) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var STATE_MODEL = MODELS.STATE;
    var SHELL_MODEL = MODELS.SHELL;
    var DETAIL_MESSAGE_KEYS = MessageKeyConstants.DETAIL;

    function CloseDetailUseCase() {
        return {
            execute: function (input, ctx) { return execute(input, ctx); }
        };
    }

function execute(mInput, mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        var oLockPort = mCtx && mCtx.lock;
        var sDbKey = DetailRuntimePayload.dbKey(mInput, mCtx) ||
            (oUiState && typeof oUiState.get === "function" && (
                oUiState.get(STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID) ||
                oUiState.get(STATE_MODEL, ModelPathContracts.SELECTED_ID)
            )) || "";
        var sSessionGuid = DetailRuntimePayload.sessionGuid(mInput, mCtx, StatePaths);
        var sEditMode = WorkflowContracts.normalizeEditMode(oUiState && typeof oUiState.get === "function" ? oUiState.get(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE) : "");
        var sLockState = WorkflowContracts.normalizeLockState(oUiState && typeof oUiState.get === "function" ? oUiState.get(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_LOCK_STATE) : "");
        var bEditableLifecycle = WorkflowContracts.isEditableMode(sEditMode) ||
            sLockState === WorkflowContracts.LOCK_STATES.EDIT_LOCKED ||
            sLockState === WorkflowContracts.LOCK_STATES.ACQUIRING_LOCK ||
            sLockState === WorkflowContracts.LOCK_STATES.IDLE_TIMEOUT_GRACE;
        var bShouldRelease = !!(
            sDbKey
            && !CreateSentinel.isCreateId(sDbKey)
            && sSessionGuid
            && oLockPort
            && typeof oLockPort.release === "function"
            && bEditableLifecycle
        );
        var aEffects;

        var pRelease = Promise.resolve();
        if (bShouldRelease) {
            pRelease = Promise.resolve(oLockPort.release(DetailRuntimePayload.lockRequest(mInput, mCtx, StatePaths))).catch(function () {
                return { ok: false, code: "ERROR", released: false, messageKey: DETAIL_MESSAGE_KEYS.LOCK_RELEASE_FAILED };
            });
        }

        return pRelease.then(function (oReleaseResult) {
            if (bShouldRelease) {
                WorkflowTelemetry.emit(
                    oReleaseResult && oReleaseResult.ok !== false && oReleaseResult.released !== false
                        ? "lock.release.completed"
                        : "lock.release.failed",
                    {
                        stateModel: mCtx && mCtx.stateModel,
                        payload: {
                            dbKey: sDbKey,
                            source: "close_detail"
                        }
                    }
                );
            }
            aEffects = [
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
                Effects.modelPatch(STATE_MODEL, ModelPathContracts.LOCK_OPERATION_PENDING, false),
                Effects.modelPatch(SHELL_MODEL, MODEL_PATHS.SHELL_LAYOUT, NavigationContracts.LAYOUTS.ONE_COLUMN),
                Effects.modelPatch(STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID, ""),
                Effects.modelPatch(STATE_MODEL, ModelPathContracts.SELECTED_ID, ""),
                Effects.modelPatch(STATE_MODEL, ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, ""),
                Effects.modelPatch(STATE_MODEL, ModelPathContracts.CURRENT_ROUTE_NAME, NavigationContracts.ROUTES.SEARCH),
                Effects.navigate(NavigationContracts.ROUTES.SEARCH, {}, true)
            ];
            if (bShouldRelease && (!oReleaseResult || oReleaseResult.ok === false || oReleaseResult.released === false)) {
                aEffects.push(Effects.warn((oReleaseResult && oReleaseResult.messageKey) || DETAIL_MESSAGE_KEYS.LOCK_RELEASE_FAILED));
            }
            return Result.ok({ reason: (mInput && mInput.intent) || "close" }, aEffects);
        });
    }

    return CloseDetailUseCase;
});
