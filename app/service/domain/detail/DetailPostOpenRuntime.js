sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts"
], function (Effects, CloneUtil, StatePaths, ModelPathContracts, WorkflowContracts, ModelContracts, DetailUseCaseConstants) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var DETAIL_MODEL_PATHS = DetailUseCaseConstants.MODEL_PATHS;

    function buildEditableDetailEffects(sDbKey, oOptions) {
        var sResolvedDbKey = String(sDbKey || "").trim();
        var oSettings = oOptions || {};
        var oSnapshot = oSettings.snapshot || {};
        var oSelectedSnapshot = CloneUtil.clone(oSnapshot, {});
        var oBaseSnapshot = CloneUtil.clone(oSnapshot, {});
        var bAutosaveEnabled = oSettings.autosaveEnabled !== false;

        return [
            Effects.modelPatch(MODELS.DETAIL, DETAIL_MODEL_PATHS.ROOT, oSelectedSnapshot),
            Effects.modelPatch(MODELS.DETAIL, DETAIL_MODEL_PATHS.BASE, oBaseSnapshot),
            Effects.modelPatch(MODELS.STATE, ModelPathContracts.ACTIVE_OBJECT_ID, sResolvedDbKey),
            Effects.modelPatch(MODELS.STATE, ModelPathContracts.SELECTED_ID, sResolvedDbKey),
            Effects.modelPatch(MODELS.STATE, ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, sResolvedDbKey),
            Effects.modelPatch(MODELS.STATE, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.EDIT),
            Effects.modelPatch(MODELS.STATE, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.EDIT_LOCKED),
            Effects.modelPatch(MODELS.STATE, StatePaths.WORKFLOW_AUTOSAVE_ENABLED, bAutosaveEnabled),
            Effects.modelPatch(MODELS.STATE, StatePaths.WORKFLOW_DIRTY, false)
        ];
    }

    return {
        buildEditableDetailEffects: buildEditableDetailEffects
    };
});
