sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPersistenceRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (ModelStateRuntime, DetailPersistenceRuntime, StatePaths, WorkflowContracts, ModelPathContracts, ModelContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;

    function resetDetailWorkflowState(oController, mPatch) {
        return ModelStateRuntime.setMany(oController, MODELS.STATE, Object.assign({
            [StatePaths.WORKFLOW_DETAIL_EDIT_MODE]: WorkflowContracts.EDIT_MODES.READ,
            [StatePaths.WORKFLOW_DETAIL_LOCK_STATE]: WorkflowContracts.LOCK_STATES.IDLE,
            "/autosaveState": WorkflowContracts.AUTOSAVE_STATES.IDLE,
            "/autosaveAt": null,
            "/autosaveEnabled": false,
            "/isDirty": false,
            [ModelPathContracts.ACTIVE_OBJECT_ID]: "",
            [ModelPathContracts.SELECTED_ID]: "",
            "/persistence": DetailPersistenceRuntime.createInitialPersistenceState()
        }, mPatch || {}));
    }

    function resetDetailRuntimeData(oController) {
        return ModelStateRuntime.replaceData(oController, MODELS.DETAIL, {
            current: {},
            base: {}
        });
    }

    return Object.freeze({
        resetDetailRuntimeData: resetDetailRuntimeData,
        resetDetailWorkflowState: resetDetailWorkflowState
    });
});
