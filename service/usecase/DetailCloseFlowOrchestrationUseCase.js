sap.ui.define([
    "sap_ui5/service/usecase/DetailCloseNavigationFlowUseCase",
    "sap_ui5/service/usecase/DetailCloseFlowUseCase",
    "sap_ui5/service/usecase/DetailCommandFlowUseCase",
    "sap_ui5/service/usecase/DetailUnsavedDecisionFlowUseCase",
    "sap_ui5/service/usecase/DetailLifecycleUseCase",
    "sap_ui5/util/FlowCoordinator"
], function (
    DetailCloseNavigationFlowUseCase,
    DetailCloseFlowUseCase,
    DetailCommandFlowUseCase,
    DetailUnsavedDecisionFlowUseCase,
    DetailLifecycleUseCase,
    FlowCoordinator
) {
    "use strict";

    function runCloseFlow(mDeps) {
        var oState = mDeps && mDeps.stateModel;
        return DetailCloseFlowUseCase.runCloseFlow({
            isDirty: oState && oState.getProperty ? oState.getProperty("/isDirty") : false,
            shouldPromptBeforeClose: DetailCommandFlowUseCase.shouldPromptBeforeClose,
            shouldProceedAfterUnsavedDecision: DetailUnsavedDecisionFlowUseCase.shouldProceedAfterDecision,
            confirmUnsaved: DetailUnsavedDecisionFlowUseCase.buildConfirmUnsavedAction({
                host: mDeps && mDeps.host,
                onSave: mDeps && mDeps.onSave,
                confirmUnsavedAndHandle: FlowCoordinator.confirmUnsavedAndHandle
            }),
            proceed: function () {
                return DetailCloseNavigationFlowUseCase.runCloseNavigation({
                    stateModel: oState,
                    releaseLock: mDeps && mDeps.releaseLock,
                    prepareCloseNavigation: DetailLifecycleUseCase.prepareCloseNavigation,
                    navigateToSearch: mDeps && mDeps.navigateToSearch
                });
            }
        }).then(function (oResult) {
            return { ok: !!(oResult && oResult.ok), reason: (oResult && oResult.reason) || "no_result" };
        });
    }

    return {
        runCloseFlow: runCloseFlow
    };
});
