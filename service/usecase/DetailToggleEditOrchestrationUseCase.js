sap.ui.define([
    "sap_ui5/service/usecase/DetailLockEditFlowUseCase",
    "sap_ui5/service/usecase/DetailUnsavedDecisionFlowUseCase",
    "sap_ui5/service/usecase/DetailLifecycleUseCase",
    "sap_ui5/util/FlowCoordinator"
], function (
    DetailLockEditFlowUseCase,
    DetailUnsavedDecisionFlowUseCase,
    DetailLifecycleUseCase,
    FlowCoordinator
) {
    "use strict";

    function runToggleFlow(mDeps) {
        var oStateModel = mDeps && mDeps.stateModel;
        var sPendingText = mDeps && mDeps.pendingText;

        return DetailLockEditFlowUseCase.runToggleEditFlow({
            editMode: !!(mDeps && mDeps.editMode),
            isDirty: oStateModel && oStateModel.getProperty ? oStateModel.getProperty("/isDirty") : false,
            confirmUnsaved: DetailUnsavedDecisionFlowUseCase.buildConfirmUnsavedAction({
                host: mDeps && mDeps.host,
                onSave: mDeps && mDeps.onSave,
                confirmUnsavedAndHandle: FlowCoordinator.confirmUnsavedAndHandle
            }),
            runPendingRelease: function () {
                mDeps.setLockUiState("PENDING", sPendingText);
                return mDeps.runWithStateFlag("/lockOperationPending", mDeps.releaseEdit);
            },
            runPendingToggle: function (fnFlow) {
                mDeps.setLockUiState("PENDING", sPendingText);
                return mDeps.runWithStateFlag("/lockOperationPending", fnFlow);
            },
            releaseEdit: mDeps && mDeps.releaseEdit,
            ensureFreshBeforeEdit: mDeps && mDeps.ensureFreshBeforeEdit,
            confirmIntegrationEdit: mDeps && mDeps.confirmIntegrationEdit,
            onStayReadOnly: function () {
                mDeps.setLockUiState("IDLE", mDeps.stayReadOnlyText);
            },
            acquireLock: mDeps && mDeps.acquireLock,
            onLockAcquired: function () {
                DetailLifecycleUseCase.setEditLocked(oStateModel);
                mDeps.setLockUiState("SUCCESS", mDeps.lockOwnedText);
            },
            tryRecoverFromAcquireError: mDeps && mDeps.tryRecoverFromAcquireError,
            onAcquireFailed: function (oError) {
                DetailLifecycleUseCase.setReadUnlocked(oStateModel);
                mDeps.setLockUiState("ERROR", mDeps.lockOwnedByOtherText);
                return mDeps.onAcquireFailed(oError);
            }
        }).then(function (oResult) {
            return { ok: oResult !== false, reason: (oResult && oResult.reason) || "applied" };
        });
    }

    return {
        runToggleFlow: runToggleFlow
    };
});
