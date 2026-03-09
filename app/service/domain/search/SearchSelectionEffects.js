sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (Effects, StatePaths) {
    "use strict";

    function buildSelectionResetEffects(mOptions) {
        var aEffects = [
            Effects.modelPatch("view", "/selectedRowId", ""),
            Effects.modelPatch("view", "/selectedRowIds", []),
            Effects.modelPatch("view", "/selectionCount", 0),
            Effects.modelPatch("view", "/hasSelection", false),
            Effects.modelPatch("view", "/canCopy", false),
            Effects.modelPatch("view", "/canDelete", false)
        ];
        if (mOptions && mOptions.markBusy) {
            aEffects.push(Effects.modelPatch("state", StatePaths.UI_BUSY_SEARCH_TABLE, true));
        }
        return aEffects;
    }

    return {
        buildSelectionResetEffects: buildSelectionResetEffects
    };
});
