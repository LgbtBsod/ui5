sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/SearchRuntimeContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiSemanticConstants"
], function (SearchViewStateRuntime, SearchRuntimeContracts, UiSemanticConstants) {
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

    function formatSearchResultsCompactText(oController, iResultCount, bHasRows) {
        var oBundle = oController.getResourceBundle && oController.getResourceBundle();
        var iSafeCount = Math.max(0, Number(iResultCount || 0));
        if (!bHasRows || !iSafeCount) {
            return (oBundle && oBundle.getText("resultsLabel")) || "Results";
        }
        return ((oBundle && oBundle.getText("resultsLabel")) || "Results") + ": " + iSafeCount;
    }

    function formatSearchSelectionSummary(oController, iSelectionCount, sSelectedRowDisplayId) {
        var oBundle = oController.getResourceBundle && oController.getResourceBundle();
        var iSafeCount = Math.max(0, Number(iSelectionCount || 0));
        var sPrimaryId = String(sSelectedRowDisplayId || "").trim();
        if (!iSafeCount) {
            return (oBundle && oBundle.getText("searchSelectionNone")) || "No selection";
        }
        if (iSafeCount === 1 && sPrimaryId) {
            return ((oBundle && oBundle.getText("searchSelectionPrimaryPrefix")) || "Primary") + ": " + sPrimaryId;
        }
        return iSafeCount + " " + ((oBundle && oBundle.getText("searchSelectionUnits")) || "selected");
    }

    function formatSelectionSummaryState() {
        return UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION;
    }

    function formatLoadErrorType() {
        return UiSemanticConstants.MESSAGE_TYPE.ERROR;
    }

    return {
        formatSearchModeChipText: formatSearchModeChipText,
        formatSearchResultsCompactText: formatSearchResultsCompactText,
        formatSearchSelectionSummary: formatSearchSelectionSummary,
        formatSelectionSummaryState: formatSelectionSummaryState,
        formatLoadErrorType: formatLoadErrorType,
        formatWorkflowStageText: formatWorkflowStageText,
        formatWorkflowStageState: formatWorkflowStageState
    };
});
