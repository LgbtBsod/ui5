sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
"PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowTelemetry",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowRuntimeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailPersistenceConstants"
], function (UseCase, Result, Effects, DetailRuntimePayload, StatePaths, CreateSentinel, WorkflowTelemetry, ModelPathContracts, NavigationContracts, WorkflowContracts, WorkflowRuntimeConstants, DetailPersistenceConstants) {
    "use strict";

    function CloseDetailUseCase() {
        UseCase.call(this, "CloseDetailUseCase");
    }

    CloseDetailUseCase.prototype = Object.create(UseCase.prototype);
    CloseDetailUseCase.prototype.constructor = CloseDetailUseCase;

    CloseDetailUseCase.prototype.execute = function (mInput, mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        var oLockPort = mCtx && mCtx.lock;
        var sRootId = DetailRuntimePayload.rootId(mInput, mCtx) ||
            (oUiState && typeof oUiState.get === "function" && (
                oUiState.get("state", ModelPathContracts.ACTIVE_OBJECT_ID) ||
                oUiState.get("state", ModelPathContracts.SELECTED_ID)
            )) || "";
        var sSessionGuid = DetailRuntimePayload.sessionGuid(mInput, mCtx, StatePaths);
        var sEditMode = WorkflowContracts.normalizeEditMode(oUiState && typeof oUiState.get === "function" ? oUiState.get("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE) : "");
        var sLockState = WorkflowContracts.normalizeLockState(oUiState && typeof oUiState.get === "function" ? oUiState.get("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE) : "");
        var bEditableLifecycle = WorkflowContracts.isEditableMode(sEditMode) ||
            sLockState === WorkflowContracts.LOCK_STATES.EDIT_LOCKED ||
            sLockState === WorkflowContracts.LOCK_STATES.ACQUIRING_LOCK ||
            sLockState === WorkflowContracts.LOCK_STATES.IDLE_TIMEOUT_GRACE;
        var bShouldRelease = !!(
            sRootId
            && !CreateSentinel.isCreateId(sRootId)
            && sSessionGuid
            && oLockPort
            && typeof oLockPort.release === "function"
            && bEditableLifecycle
        );
        var aEffects;

        var pRelease = Promise.resolve();
        if (bShouldRelease) {
            pRelease = Promise.resolve(oLockPort.release(DetailRuntimePayload.lockRequest(mInput, mCtx, StatePaths))).catch(function () {
                return { ok: false, code: "ERROR", released: false, messageKey: "lockReleaseFailed" };
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
                            rootId: sRootId,
                            source: "close_detail"
                        }
                    }
                );
            }
            aEffects = [
                Effects.modelPatch("state", StatePaths.READINESS_DETAIL, {
                    status: WorkflowRuntimeConstants.READINESS_STATUS.IDLE,
                    ready: false,
                    readyAt: "",
                    error: "",
                    rootId: "",
                    mode: WorkflowContracts.EDIT_MODES.READ,
                    permissionKnown: false,
                    lockKnown: false
                }),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.IDLE),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.IDLE),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null),
                Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
                Effects.modelPatch("state", StatePaths.SAVE_IN_FLIGHT, false),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, false),
                Effects.modelPatch("state", StatePaths.PERSISTENCE_STATE, DetailPersistenceConstants.STATES.IDLE),
                Effects.modelPatch("state", StatePaths.PERSISTENCE_NEXT_HEARTBEAT_AT, null),
                Effects.modelPatch("state", StatePaths.PERSISTENCE_HAS_VALID_LOCK, false),
                Effects.modelPatch("state", StatePaths.PERSISTENCE_LOCK_OWNER_SESSION_MATCHES, false),
                Effects.modelPatch("state", ModelPathContracts.LOCK_OPERATION_PENDING, false),
                Effects.modelPatch("state", ModelPathContracts.LAYOUT, NavigationContracts.LAYOUTS.ONE_COLUMN),
                Effects.modelPatch("state", ModelPathContracts.ACTIVE_OBJECT_ID, ""),
                Effects.modelPatch("state", ModelPathContracts.SELECTED_ID, ""),
                Effects.modelPatch("state", ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, ""),
                Effects.modelPatch("state", ModelPathContracts.CURRENT_ROUTE_NAME, NavigationContracts.ROUTES.SEARCH),
                Effects.navigate(NavigationContracts.ROUTES.SEARCH, {}, true)
            ];
            if (bShouldRelease && (!oReleaseResult || oReleaseResult.ok === false || oReleaseResult.released === false)) {
                aEffects.push(Effects.warn((oReleaseResult && oReleaseResult.messageKey) || "lockReleaseFailed"));
            }
            return Result.ok({ reason: (mInput && mInput.intent) || "close" }, aEffects);
        });
    };

    return CloseDetailUseCase;
});
