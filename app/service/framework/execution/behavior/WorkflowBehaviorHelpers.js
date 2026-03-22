sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DialogOrchestrator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailResetRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (DialogOrchestrator, FeedbackCoordinator, ModelStateRuntime, DetailResetRuntime, StatePaths, WorkflowContracts, ModelPathContracts, ModelContracts) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;

    function resolveText(oController, sKey, aArgs, sFallback) {
        return FeedbackCoordinator.resolveText(oController, sKey, aArgs || [], sFallback || sKey);
    }

    function resetDetailWorkflowState(oController) {
        DetailResetRuntime.resetDetailWorkflowState(oController, {
            [StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE]: ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.IDLE) || WorkflowContracts.AUTOSAVE_STATES.IDLE,
            [StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT]: ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null),
            [StatePaths.WORKFLOW_AUTOSAVE_ENABLED]: ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false) === true,
            [ModelPathContracts.ACTIVE_OBJECT_ID]: "",
            [ModelPathContracts.SELECTED_ID]: "",
            [ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID]: ""
        });
    }

    function promptWarning(oController, sTextKey, aActions, sInitialAction, aArgs, sFallback) {
        return DialogOrchestrator.promptWarning(
            resolveText(oController, sTextKey, aArgs || [], sFallback || sTextKey),
            aActions,
            sInitialAction
        );
    }

    function promptError(oController, sTextKey, aArgs, sFallback) {
        DialogOrchestrator.promptError(resolveText(oController, sTextKey, aArgs || [], sFallback || sTextKey));
    }

    return Object.freeze({
        resolveText: resolveText,
        resetDetailWorkflowState: resetDetailWorkflowState,
        promptWarning: promptWarning,
        promptError: promptError
    });
});
