sap.ui.define([
    "checklist/app/service/framework/Effects",
    "checklist/app/model/StatePaths"
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
