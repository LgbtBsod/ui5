sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants"
], function (Effects, StatePaths, ModelPathContracts, WorkflowContracts) {
    "use strict";

    function buildEditableDetailEffects(sRootId, oOptions) {
        var sResolvedRootId = String(sRootId || "").trim();
        var oSettings = oOptions || {};
        var oSnapshot = oSettings.snapshot || {};
        var bAutosaveEnabled = oSettings.autosaveEnabled !== false;

        return [
            Effects.modelPatch("selected", "/", oSnapshot),
            Effects.modelPatch("snapshot", "/", oSnapshot),
            Effects.modelPatch("state", ModelPathContracts.ACTIVE_OBJECT_ID, sResolvedRootId),
            Effects.modelPatch("state", ModelPathContracts.SELECTED_ID, sResolvedRootId),
            Effects.modelPatch("state", ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, sResolvedRootId),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.EDIT),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.EDIT_LOCKED),
            Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, bAutosaveEnabled),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, false)
        ];
    }

    return {
        buildEditableDetailEffects: buildEditableDetailEffects
    };
});
