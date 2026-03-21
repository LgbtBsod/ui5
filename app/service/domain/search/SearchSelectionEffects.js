sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (Effects, StatePaths, ModelContracts) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var VIEW_MODEL = ModelContracts.MODELS.VIEW;

    function buildSelectionResetEffects(mOptions) {
        var aEffects = [
            Effects.modelPatch(VIEW_MODEL, "/selectedRowId", ""),
            Effects.modelPatch(VIEW_MODEL, "/selectedRowDisplayId", ""),
            Effects.modelPatch(VIEW_MODEL, "/selectedRowIds", []),
            Effects.modelPatch(VIEW_MODEL, "/selectionCount", 0),
            Effects.modelPatch(VIEW_MODEL, "/hasSelection", false),
            Effects.modelPatch(VIEW_MODEL, "/canCopy", false),
            Effects.modelPatch(VIEW_MODEL, "/canDelete", false)
        ];
        if (mOptions && mOptions.markBusy) {
            aEffects.push(Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_SEARCH_TABLE, true));
        }
        return aEffects;
    }

    return {
        buildSelectionResetEffects: buildSelectionResetEffects
    };
});
