sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DialogOrchestrator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/FeedbackBehaviorHelpers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/runtime/DetailEditSessionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (DialogOrchestrator, FeedbackBehaviorHelpers, ModelStateRuntime, DetailEditSessionRuntime, StatePaths, WorkflowContracts, ModelContracts) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var oDetailEditSessionRuntime = new DetailEditSessionRuntime();

    function resolveText(oController, sKey, aArgs, sFallback) {
        return FeedbackBehaviorHelpers.resolveText(oController, sKey, aArgs || [], sFallback || sKey);
    }

    function resetDetailWorkflowState(oController) {
        oDetailEditSessionRuntime.resetDetailWorkflowState(oController, {
            [StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE]: ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.IDLE) || WorkflowContracts.AUTOSAVE_STATES.IDLE,
            [StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT]: ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null),
            [StatePaths.WORKFLOW_AUTOSAVE_ENABLED]: ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false) === true,
            [StatePaths.ACTIVE_OBJECT_ID]: "",
            [StatePaths.SELECTED_ID]: "",
            [StatePaths.POST_OPEN_HYDRATED_ROOT_ID]: ""
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
