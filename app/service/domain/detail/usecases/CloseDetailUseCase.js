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
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts"
], function (UseCase, Result, Effects, DetailRuntimePayload, StatePaths, CreateSentinel, WorkflowTelemetry, ModelPathContracts, NavigationContracts, WorkflowContracts) {
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
            if (sRootId && !CreateSentinel.isCreateId(sRootId) && sSessionGuid) {
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
                    status: "idle",
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
                Effects.modelPatch("state", ModelPathContracts.LOCK_OPERATION_PENDING, false),
                Effects.modelPatch("state", ModelPathContracts.LAYOUT, NavigationContracts.LAYOUTS.ONE_COLUMN),
                Effects.modelPatch("state", ModelPathContracts.ACTIVE_OBJECT_ID, null),
                Effects.modelPatch("state", ModelPathContracts.SELECTED_ID, null),
                Effects.navigate(NavigationContracts.ROUTES.SEARCH, {}, true)
            ];
            if (sRootId && !CreateSentinel.isCreateId(sRootId) && sSessionGuid && (!oReleaseResult || oReleaseResult.ok === false || oReleaseResult.released === false)) {
                aEffects.push(Effects.warn((oReleaseResult && oReleaseResult.messageKey) || "lockReleaseFailed"));
            }
            return Result.ok({ reason: (mInput && mInput.intent) || "close" }, aEffects);
        });
    };

    return CloseDetailUseCase;
});
