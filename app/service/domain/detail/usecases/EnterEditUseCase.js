sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAuthorizationRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowTelemetry",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowRuntimeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailUseCaseConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailMessageKeyConstants"
], function (Result, Effects, DetailAuthorizationRuntime, DetailRuntimePayload, UseCaseValue, StatePaths, ModelStateRuntime, CreateSentinel, WorkflowTelemetry, WorkflowContracts, WorkflowRuntimeConstants, ModelContracts, DetailUseCaseConstants, DetailMessageKeyConstants) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var DETAIL_CODES = DetailUseCaseConstants.CODES;
    var ACCESS_REASON_CODES = DetailUseCaseConstants.ACCESS_REASON_CODES;
    var DETAIL_MESSAGE_KEYS = DetailMessageKeyConstants;

    function EnterEditUseCase() {
        return {
            execute: execute
        };
    }

function readCode(oLock) {
        return String((oLock && oLock.code) || "").toUpperCase();
    }

    function readOnlyEffects() {
        return [
            Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ),
            Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY),
            Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
        ];
    }

    function execute(mInput, mCtx) {
        var bEdit = !!(mInput && mInput.state);
        var oLockPort = mCtx && mCtx.lock;
        var sRootId = UseCaseValue.rootId(mInput);
        var sSessionGuid = DetailRuntimePayload.sessionGuid(mInput, mCtx, StatePaths);
        var bCreateDraft = CreateSentinel.isCreateId(sRootId);
        var bShouldRelease = !!(sRootId && !CreateSentinel.isCreateId(sRootId) && oLockPort && typeof oLockPort.release === "function" && sSessionGuid);
        var oStateModel = mCtx && mCtx.stateModel;

        if (!bEdit) {
            var pRelease = Promise.resolve();
            if (bShouldRelease) {
                pRelease = Promise.resolve(oLockPort.release(DetailRuntimePayload.lockRequest(mInput, mCtx, StatePaths))).catch(function () { return null; });
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
                                source: WorkflowRuntimeConstants.SOURCES.ENTER_EDIT
                            }
                        }
                    );
                }
                return Result.ok({ code: WorkflowContracts.EDIT_MODES.READ }, readOnlyEffects());
            });
        }
        if (bCreateDraft) {
            return Promise.resolve(Result.ok({ code: ACCESS_REASON_CODES.CREATE_DRAFT }, [
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.CREATE),
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.IDLE),
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
            ]));
        }
        if (!sSessionGuid) {
            return Promise.resolve(Result.fail({ message: "Session unavailable", code: "SESSION_UNAVAILABLE" }, [Effects.warn("sessionUnavailableMessage")]));
        }
        if (!sRootId || !oLockPort || typeof oLockPort.acquire !== "function") {
            return Promise.resolve(Result.fail({ message: "Lock port unavailable", code: "PORT_UNAVAILABLE" }));
        }

        var oCacheValidation = mCtx && mCtx.cacheValidation;

        if (oStateModel) {
            ModelStateRuntime.writeOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.ACQUIRING_LOCK);
        }

        return DetailAuthorizationRuntime.fetchPermission(mCtx || {}, sRootId, {
            activity: DetailAuthorizationRuntime.OPERATIONS.CHANGE
        }).then(function (oPermission) {
            var pPrecheck;
            if (!oPermission.allowed) {
                return Result.fail({ message: "No permission to edit checklist", code: DETAIL_CODES.NO_EDIT_PERMISSION }, DetailAuthorizationRuntime.deniedActionEffects(oPermission, "detailEditPermissionDenied", readOnlyEffects()));
            }
            pPrecheck = (oCacheValidation && typeof oCacheValidation.execute === "function")
                ? Promise.resolve(oCacheValidation.execute({ rootId: sRootId, toleranceMs: 5500 }, mCtx || {})).catch(function () { return null; })
                : Promise.resolve(null);
            return pPrecheck.then(function (oValidation) {
                var oValidationData = (oValidation && oValidation.ok && oValidation.data) ? oValidation.data : null;
                if (oValidationData && oValidationData.invalidated) {
            return Result.fail({ message: "Cache invalidated; retry edit", code: "CACHE_INVALIDATED" }, DetailAuthorizationRuntime.contentAccessEffects(oPermission).concat(readOnlyEffects()));
                }
                return Promise.resolve(oLockPort.acquire(DetailRuntimePayload.lockRequest(mInput, mCtx, StatePaths))).then(function (oLock) {
                    return {
                        lock: oLock,
                        permission: oPermission
                    };
                });
            });
        }).then(function (oResolved) {
            if (oResolved && oResolved.ok === false) {
                return oResolved;
            }
            var oLock = oResolved && oResolved.lock;
            var oPermission = oResolved && oResolved.permission;
            var sCode = readCode(oLock);
            if (oLock && oLock.ok && sCode !== DETAIL_CODES.KILLED) {
                WorkflowTelemetry.emit("lock.acquire.success", {
                    stateModel: mCtx && mCtx.stateModel,
                    payload: {
                        rootId: sRootId,
                            source: WorkflowRuntimeConstants.SOURCES.ENTER_EDIT
                    }
                });
                return Result.ok({ code: DETAIL_CODES.OK, lock: oLock }, DetailAuthorizationRuntime.contentAccessEffects(oPermission).concat([
                    Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.EDIT),
                    Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.EDIT_LOCKED),
                    Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_AUTOSAVE_ENABLED, true),
                ]));
            }
            if (sCode === DETAIL_CODES.LOCKED_OWN_SESSION) {
                WorkflowTelemetry.emit("lock.acquire.failed", {
                    stateModel: mCtx && mCtx.stateModel,
                    payload: { rootId: sRootId, source: WorkflowRuntimeConstants.SOURCES.ENTER_EDIT, code: sCode }
                });
            return Result.fail({ message: "Lock held by own session", code: DETAIL_CODES.LOCKED_OWN_SESSION, legacyCode: DETAIL_CODES.LOCK_ACQUIRE_FAILED, lock: oLock || {} }, DetailAuthorizationRuntime.contentAccessEffects(oPermission).concat(readOnlyEffects()));
            }
            if (sCode === DETAIL_CODES.EXPIRED) {
                WorkflowTelemetry.emit("lock.acquire.failed", {
                    stateModel: mCtx && mCtx.stateModel,
                    payload: { rootId: sRootId, source: WorkflowRuntimeConstants.SOURCES.ENTER_EDIT, code: sCode }
                });
            return Result.fail({ message: "Lock expired", code: DETAIL_CODES.EXPIRED, legacyCode: DETAIL_CODES.LOCK_ACQUIRE_FAILED, lock: oLock || {} }, DetailAuthorizationRuntime.contentAccessEffects(oPermission).concat(readOnlyEffects()));
            }
            if (sCode === DETAIL_CODES.KILLED) {
                WorkflowTelemetry.emit("lock.acquire.failed", {
                    stateModel: mCtx && mCtx.stateModel,
                    payload: { rootId: sRootId, source: WorkflowRuntimeConstants.SOURCES.ENTER_EDIT, code: sCode }
                });
            return Result.fail({ message: "Lock killed", code: DETAIL_CODES.KILLED, legacyCode: DETAIL_CODES.LOCK_ACQUIRE_FAILED, lock: oLock || {} }, DetailAuthorizationRuntime.contentAccessEffects(oPermission).concat(readOnlyEffects(), [Effects.warn(DETAIL_MESSAGE_KEYS.LOCK_KILLED)]));
            }
            WorkflowTelemetry.emit("lock.acquire.failed", {
                stateModel: mCtx && mCtx.stateModel,
                payload: { rootId: sRootId, source: WorkflowRuntimeConstants.SOURCES.ENTER_EDIT, code: sCode || DETAIL_CODES.LOCKED }
            });
            return Result.fail({ message: "Lock acquire failed", code: DETAIL_CODES.LOCKED, legacyCode: DETAIL_CODES.LOCK_ACQUIRE_FAILED, lock: oLock || {} }, DetailAuthorizationRuntime.contentAccessEffects(oPermission).concat(readOnlyEffects(), [Effects.warn(DETAIL_MESSAGE_KEYS.LOCK_ACQUIRE_FAILED)]));
        }).catch(function (oError) {
            WorkflowTelemetry.emit("lock.acquire.failed", {
                stateModel: mCtx && mCtx.stateModel,
                payload: { rootId: sRootId, source: WorkflowRuntimeConstants.SOURCES.ENTER_EDIT, code: String((oError && oError.code) || DETAIL_CODES.TECHNICAL_ERROR) }
            });
            return Result.fail(oError, readOnlyEffects());
        });
    }

    return EnterEditUseCase;
});
