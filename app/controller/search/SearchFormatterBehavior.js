sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiSemanticConstants"
], function (SearchViewStateRuntime, SearchContracts, UiSemanticConstants) {
    "use strict";

    var SEARCH_MODE = SearchContracts.SEARCH_MODE;

    function resolveBundleText(oController, sKey) {
        var oBundle = oController && oController.getResourceBundle && oController.getResourceBundle();
        if (!sKey || !oBundle || !oBundle.getText) {
            return "";
        }
        return String(oBundle.getText(sKey) || "");
    }

    function formatSearchModeChipText(oController, sMode) {
        var sNorm = String(sMode || "").toUpperCase() === SEARCH_MODE.LOOSE ? SEARCH_MODE.LOOSE : SEARCH_MODE.EXACT;
        var sLabel = resolveBundleText(oController, SearchContracts.SEARCH_MODE_LABEL);
        var sModeText = sNorm === SEARCH_MODE.LOOSE
            ? resolveBundleText(oController, SearchContracts.SEARCH_MODE_LOOSE)
            : resolveBundleText(oController, SearchContracts.SEARCH_MODE_EXACT);
        return sLabel + ": " + sModeText;
    }

    function formatWorkflowStageText(oController, sStage) {
        return SearchViewStateRuntime.formatWorkflowStageText(
            oController && oController.getResourceBundle && oController.getResourceBundle(),
            sStage
        );
    }

    function formatWorkflowStageState(sStage) {
        return SearchViewStateRuntime.formatWorkflowStageState(sStage);
    }

    function formatSearchResultsCompactText(oController, iResultCount, bHasRows) {
        var iSafeCount = Math.max(0, Number(iResultCount || 0));
        var sResultsLabel = resolveBundleText(oController, SearchContracts.RESULTS_LABEL);
        if (!bHasRows || !iSafeCount) {
            return sResultsLabel;
        }
        return sResultsLabel + ": " + iSafeCount;
    }

    function formatSearchSelectionSummary(oController, iSelectionCount, sSelectedRowDisplayId) {
        var iSafeCount = Math.max(0, Number(iSelectionCount || 0));
        var sPrimaryId = String(sSelectedRowDisplayId || "").trim();
        if (!iSafeCount) {
            return resolveBundleText(oController, SearchContracts.SEARCH_SELECTION_NONE);
        }
        if (iSafeCount === 1 && sPrimaryId) {
            return resolveBundleText(oController, SearchContracts.SEARCH_SELECTION_PRIMARY_PREFIX) + ": " + sPrimaryId;
        }
        return iSafeCount + " " + resolveBundleText(oController, SearchContracts.SEARCH_SELECTION_UNITS);
    }

    function formatSelectionSummaryState() {
        return UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION;
    }

    function formatLoadErrorType() {
        return UiSemanticConstants.MESSAGE_TYPE.ERROR;
    }

    function formatSearchModeState() {
        return UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION;
    }

    function formatToolbarSelectionState(iSelectionCount) {
        return Number(iSelectionCount || 0) > 0
            ? UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS
            : UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION;
    }

    return {
        formatSearchModeChipText: formatSearchModeChipText,
        formatSearchResultsCompactText: formatSearchResultsCompactText,
        formatSearchSelectionSummary: formatSearchSelectionSummary,
        formatSelectionSummaryState: formatSelectionSummaryState,
        formatLoadErrorType: formatLoadErrorType,
        formatSearchModeState: formatSearchModeState,
        formatToolbarSelectionState: formatToolbarSelectionState,
        formatWorkflowStageText: formatWorkflowStageText,
        formatWorkflowStageState: formatWorkflowStageState
    };
});
