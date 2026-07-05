sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPersistenceRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants"
], function (ModelStateRuntime, DetailPersistenceRuntime, StatePaths, WorkflowContracts, ModelContracts, MessageKeyConstants) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;

    function markDirty(oController) {
        var aEffects = DetailPersistenceRuntime.dirtyEffects(true);
        var oPersistencePatch = (aEffects || []).filter(function (oEffect) {
            return oEffect && oEffect.type === "modelPatch" && oEffect.path === "/persistence";
        })[0];

        ModelStateRuntime.write(oController, STATE_MODEL, StatePaths.WORKFLOW_DIRTY, true);

        if (oPersistencePatch) {
            ModelStateRuntime.write(oController, STATE_MODEL, "/persistence", oPersistencePatch.value);
            return;
        }

        ModelStateRuntime.write(oController, STATE_MODEL, "/persistence/state", WorkflowContracts.PERSISTENCE_STATES.DIRTY);
        ModelStateRuntime.write(oController, STATE_MODEL, "/persistence/messageKey", MessageKeyConstants.VIEW.PERSISTENCE_DIRTY);
    }

    function resetDetailWorkflowState(oController, mPatch) {
        return ModelStateRuntime.setMany(oController, STATE_MODEL, Object.assign({
            [StatePaths.WORKFLOW_DETAIL_EDIT_MODE]: WorkflowContracts.EDIT_MODES.READ,
            [StatePaths.WORKFLOW_DETAIL_LOCK_STATE]: WorkflowContracts.LOCK_STATES.IDLE,
            "/autosaveState": WorkflowContracts.AUTOSAVE_STATES.IDLE,
            "/autosaveAt": null,
            "/autosaveEnabled": false,
            "/isDirty": false,
            [StatePaths.ACTIVE_OBJECT_ID]: "",
            [StatePaths.SELECTED_ID]: "",
            "/persistence": DetailPersistenceRuntime.createInitialPersistenceState()
        }, mPatch || {}));
    }

    function resetDetailRuntimeData(oController) {
        return ModelStateRuntime.replaceData(oController, ModelContracts.MODELS.DETAIL, {
            current: {},
            base: {}
        });
    }

    return Object.freeze({
        markDirty: markDirty,
        resetDetailWorkflowState: resetDetailWorkflowState,
        resetDetailRuntimeData: resetDetailRuntimeData
    });
});
