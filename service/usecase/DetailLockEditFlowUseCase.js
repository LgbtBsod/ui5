sap.ui.define([
    "sap_ui5/service/usecase/DetailEditOrchestrationUseCase",
    "sap_ui5/service/usecase/DetailCommandFlowUseCase"
], function (DetailEditOrchestrationUseCase, DetailCommandFlowUseCase) {
    "use strict";

    function runToggleEditFlow(mArgs) {
        return DetailEditOrchestrationUseCase.runToggleEditFlow({
            editMode: mArgs.editMode,
            isDirty: mArgs.isDirty,
            shouldPromptBeforeDisableEdit: DetailCommandFlowUseCase.shouldPromptBeforeDisableEdit,
            isCancelDecision: DetailCommandFlowUseCase.isCancelDecision,
            confirmUnsaved: mArgs.confirmUnsaved,
            runPendingRelease: mArgs.runPendingRelease,
            runPendingToggle: mArgs.runPendingToggle,
            releaseEdit: mArgs.releaseEdit,
            ensureFreshBeforeEdit: mArgs.ensureFreshBeforeEdit,
            confirmIntegrationEdit: mArgs.confirmIntegrationEdit,
            onStayReadOnly: mArgs.onStayReadOnly,
            acquireLock: mArgs.acquireLock,
            onLockAcquired: mArgs.onLockAcquired,
            tryRecoverFromAcquireError: mArgs.tryRecoverFromAcquireError,
            onAcquireFailed: mArgs.onAcquireFailed
        });
    }

    return {
        runToggleEditFlow: runToggleEditFlow
    };
});
