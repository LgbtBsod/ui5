sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/EnterEditUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/CloseDetailUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/LockLostUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/ForceReadOnlyUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/lock/usecases/TakeoverLockUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailWorkflowRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailDirtyStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailEditRestoreRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailResetRuntime"
], function (
    EnterEditUseCase,
    CloseDetailUseCase,
    LockLostUseCase,
    ForceReadOnlyUseCase,
    TakeoverLockUseCase,
    DetailWorkflowRuntime,
    DetailDirtyStateRuntime,
    DetailEditRestoreRuntime,
    DetailResetRuntime
) {
    "use strict";

    function executeUseCase(oUseCase, mInput, mCtx) {
        return oUseCase.execute(mInput || {}, mCtx || {});
    }

    function DetailEditSessionRuntime(mDeps) {
        var d = mDeps || {};
        this._uc = {
            enterEdit: d.enterEditUseCase || EnterEditUseCase(),
            close: d.closeUseCase || CloseDetailUseCase(),
            lockLost: d.lockLostUseCase || LockLostUseCase(),
            forceReadOnly: d.forceReadOnlyUseCase || ForceReadOnlyUseCase(),
            takeoverLock: d.takeoverLockUseCase || TakeoverLockUseCase()
        };
    }

    DetailEditSessionRuntime.prototype.enterEdit = function (mInput, mCtx) {
        return Promise.resolve(executeUseCase(this._uc.enterEdit, mInput, mCtx)).then(function (oResult) {
            return DetailWorkflowRuntime.decorateEnterEditResult(oResult, mInput);
        });
    };

    DetailEditSessionRuntime.prototype.confirmTakeover = function (mInput, mCtx) {
        return executeUseCase(this._uc.takeoverLock, mInput, mCtx);
    };

    DetailEditSessionRuntime.prototype.cancelEnterEdit = function () {
        return DetailWorkflowRuntime.buildCancelEnterEditResult();
    };

    DetailEditSessionRuntime.prototype.discardChanges = function (_mInput, mCtx) {
        return Promise.resolve({
            ok: true,
            effects: DetailWorkflowRuntime.buildDiscardEffects(mCtx && mCtx.uiState)
        });
    };

    DetailEditSessionRuntime.prototype.onLockLost = function (mInput, mCtx) {
        return executeUseCase(this._uc.lockLost, mInput, mCtx);
    };

    DetailEditSessionRuntime.prototype.forceReadOnly = function (mInput, mCtx) {
        return executeUseCase(this._uc.forceReadOnly, mInput, mCtx);
    };

    DetailEditSessionRuntime.prototype.close = function (mInput, mCtx) {
        return executeUseCase(this._uc.close, mInput, mCtx);
    };

    DetailEditSessionRuntime.prototype.markDirty = function (oController) {
        return DetailDirtyStateRuntime.markDirty(oController);
    };

    DetailEditSessionRuntime.prototype.clearAnalyticsReturnRestore = function (oController) {
        return DetailEditRestoreRuntime.clearAnalyticsReturnRestore(oController);
    };

    DetailEditSessionRuntime.prototype.restoreAnalyticsEditIfNeeded = function (oController, sDbKey, mHooks) {
        return DetailEditRestoreRuntime.restoreAnalyticsEditIfNeeded(oController, sDbKey, mHooks);
    };

    DetailEditSessionRuntime.prototype.resetDetailWorkflowState = function (oController, mPatch) {
        return DetailResetRuntime.resetDetailWorkflowState(oController, mPatch);
    };

    DetailEditSessionRuntime.prototype.resetDetailRuntimeData = function (oController) {
        return DetailResetRuntime.resetDetailRuntimeData(oController);
    };

    return DetailEditSessionRuntime;
});
