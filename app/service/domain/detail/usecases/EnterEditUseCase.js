sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAuthorizationSupport",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseInputUtils",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/util/WorkflowTelemetry"
], function (UseCase, Result, Effects, DetailAuthorizationSupport, DetailRuntimePayload, UseCaseInputUtils, StatePaths, CreateSentinel, WorkflowTelemetry) {
    "use strict";

    function EnterEditUseCase() {
        UseCase.call(this, "EnterEditUseCase");
    }

    EnterEditUseCase.prototype = Object.create(UseCase.prototype);
    EnterEditUseCase.prototype.constructor = EnterEditUseCase;

    function readCode(oLock) {
        return String((oLock && oLock.code) || "").toUpperCase();
    }

    function readOnlyEffects() {
        return [
            Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "READ"),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "IDLE"),
            Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
        ];
    }

    EnterEditUseCase.prototype.execute = function (mInput, mCtx) {
        var bEdit = !!(mInput && mInput.state);
        var oLockPort = mCtx && mCtx.lock;
        var sRootId = UseCaseInputUtils.rootId(mInput);
        var sSessionGuid = DetailRuntimePayload.sessionGuid(mInput, mCtx, StatePaths);
        var bCreateDraft = CreateSentinel.isCreateId(sRootId);
        var bShouldRelease = !!(sRootId && !CreateSentinel.isCreateId(sRootId) && oLockPort && typeof oLockPort.release === "function" && sSessionGuid);

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
                                source: "enter_edit_toggle"
                            }
                        }
                    );
                }
                return Result.ok({ code: "READ" }, readOnlyEffects());
            });
        }
        if (bCreateDraft) {
            return Promise.resolve(Result.ok({ code: "CREATE_DRAFT" }, [
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "CREATE"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "IDLE"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
            ]));
        }
        if (!sSessionGuid) {
            return Promise.resolve(Result.fail({ message: "Session unavailable", code: "SESSION_UNAVAILABLE" }, [Effects.warn("sessionUnavailableMessage")]));
        }
        if (!sRootId || !oLockPort || typeof oLockPort.acquire !== "function") {
            return Promise.resolve(Result.fail({ message: "Lock port unavailable", code: "PORT_UNAVAILABLE" }));
        }

        var oCacheValidation = mCtx && mCtx.cacheValidation;

        return DetailAuthorizationSupport.fetchPermission(mCtx || {}, sRootId, {
            activity: DetailAuthorizationSupport.OPERATIONS.CHANGE
        }).then(function (oPermission) {
            var pPrecheck;
            if (!oPermission.allowed) {
                return Result.fail({ message: "No permission to edit checklist", code: "NO_EDIT_PERMISSION" }, DetailAuthorizationSupport.deniedActionEffects(oPermission, "detailEditPermissionDenied", readOnlyEffects()));
            }
            pPrecheck = (oCacheValidation && typeof oCacheValidation.execute === "function")
                ? Promise.resolve(oCacheValidation.execute({ rootId: sRootId, toleranceMs: 5500 }, mCtx || {})).catch(function () { return null; })
                : Promise.resolve(null);
            return pPrecheck.then(function (oValidation) {
                var oValidationData = (oValidation && oValidation.ok && oValidation.data) ? oValidation.data : null;
                if (oValidationData && oValidationData.invalidated) {
                    return Result.fail({ message: "Cache invalidated; retry edit", code: "CACHE_INVALIDATED" }, DetailAuthorizationSupport.contentAccessEffects(oPermission).concat(readOnlyEffects()));
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
            if (oLock && oLock.ok && sCode !== "KILLED") {
                WorkflowTelemetry.emit("lock.acquire.success", {
                    stateModel: mCtx && mCtx.stateModel,
                    payload: {
                        rootId: sRootId,
                        source: "enter_edit"
                    }
                });
                return Result.ok({ code: "OK", lock: oLock }, DetailAuthorizationSupport.contentAccessEffects(oPermission).concat([
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "EDIT"),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "LOCKED"),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, true),
                ]));
            }
            if (sCode === "LOCKED_OWN_SESSION") {
                WorkflowTelemetry.emit("lock.acquire.failed", {
                    stateModel: mCtx && mCtx.stateModel,
                    payload: { rootId: sRootId, source: "enter_edit", code: sCode }
                });
                return Result.fail({ message: "Lock held by own session", code: "LOCKED_OWN_SESSION", legacyCode: "LOCK_ACQUIRE_FAILED", lock: oLock || {} }, DetailAuthorizationSupport.contentAccessEffects(oPermission).concat(readOnlyEffects()));
            }
            if (sCode === "EXPIRED") {
                WorkflowTelemetry.emit("lock.acquire.failed", {
                    stateModel: mCtx && mCtx.stateModel,
                    payload: { rootId: sRootId, source: "enter_edit", code: sCode }
                });
                return Result.fail({ message: "Lock expired", code: "EXPIRED", legacyCode: "LOCK_ACQUIRE_FAILED", lock: oLock || {} }, DetailAuthorizationSupport.contentAccessEffects(oPermission).concat(readOnlyEffects()));
            }
            if (sCode === "KILLED") {
                WorkflowTelemetry.emit("lock.acquire.failed", {
                    stateModel: mCtx && mCtx.stateModel,
                    payload: { rootId: sRootId, source: "enter_edit", code: sCode }
                });
                return Result.fail({ message: "Lock killed", code: "KILLED", legacyCode: "LOCK_ACQUIRE_FAILED", lock: oLock || {} }, DetailAuthorizationSupport.contentAccessEffects(oPermission).concat(readOnlyEffects(), [Effects.warn("lockKilledMessage")]));
            }
            WorkflowTelemetry.emit("lock.acquire.failed", {
                stateModel: mCtx && mCtx.stateModel,
                payload: { rootId: sRootId, source: "enter_edit", code: sCode || "LOCKED" }
            });
            return Result.fail({ message: "Lock acquire failed", code: "LOCKED", legacyCode: "LOCK_ACQUIRE_FAILED", lock: oLock || {} }, DetailAuthorizationSupport.contentAccessEffects(oPermission).concat(readOnlyEffects(), [Effects.warn("lockAcquireFailed")]));
        }).catch(function (oError) {
            WorkflowTelemetry.emit("lock.acquire.failed", {
                stateModel: mCtx && mCtx.stateModel,
                payload: { rootId: sRootId, source: "enter_edit", code: String((oError && oError.code) || "ERROR") }
            });
            return Result.fail(oError, readOnlyEffects());
        });
    };

    return EnterEditUseCase;
});
