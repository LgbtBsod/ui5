sap.ui.define([
    "sap_ui5/util/ChecklistUiState"
], function (ChecklistUiState) {
    "use strict";

    function applyValidationState(oViewModel, oValidation, fnBuildValidationMap) {
        if (!oViewModel) {
            return;
        }

        oViewModel.setProperty("/validationShown", !oValidation.valid);
        oViewModel.setProperty("/validationMissing", fnBuildValidationMap(oValidation.missingPaths));
    }

    function resolveValidationWarningCount(oValidation) {
        return oValidation.missingPaths.length + (oValidation.hasAtLeastOneCheck ? 0 : 1);
    }

    function markDirtyStatusAndNormalize(oSelectedModel, oStateModel, sTargetStatus) {
        oSelectedModel.setProperty("/root/status", ChecklistUiState.normalizeStatus(sTargetStatus));
        oStateModel.setProperty("/isDirty", true);
    }

    return {
        applyValidationState: applyValidationState,
        resolveValidationWarningCount: resolveValidationWarningCount,
        markDirtyStatusAndNormalize: markDirtyStatusAndNormalize
    };
});
