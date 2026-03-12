sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/SearchRuntimeContracts"
], function (SearchViewStateRuntime, SearchRuntimeContracts) {
    "use strict";

    var SEARCH_MODE = SearchRuntimeContracts.SEARCH_MODE;

    function formatSearchModeChipText(oController, sMode) {
        var oBundle = oController.getResourceBundle && oController.getResourceBundle();
        var sNorm = String(sMode || "").toUpperCase() === SEARCH_MODE.LOOSE ? SEARCH_MODE.LOOSE : SEARCH_MODE.EXACT;
        var sLabel = oBundle && oBundle.getText("searchModeLabel") || "Mode";
        var sModeText = sNorm === SEARCH_MODE.LOOSE
            ? (oBundle && oBundle.getText("searchModeLoose") || "Loose")
            : (oBundle && oBundle.getText("searchModeExact") || "Exact");
        return sLabel + ": " + sModeText;
    }

    function formatWorkflowStageText(oController, sStage) {
        return SearchViewStateRuntime.formatWorkflowStageText(
            oController.getResourceBundle && oController.getResourceBundle(),
            sStage
        );
    }

    function formatWorkflowStageState(sStage) {
        return SearchViewStateRuntime.formatWorkflowStageState(sStage);
    }

    return {
        formatSearchModeChipText: formatSearchModeChipText,
        formatWorkflowStageText: formatWorkflowStageText,
        formatWorkflowStageState: formatWorkflowStageState
    };
});
