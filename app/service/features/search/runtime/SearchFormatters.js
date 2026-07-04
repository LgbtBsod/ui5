sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime"
], function (SearchContracts, SearchViewStateRuntime) {
    "use strict";

    var SEARCH_MODE = SearchContracts.SEARCH_MODE;

    function resolveBundleText(oController, sKey) {
        var oBundle = oController && oController.getResourceBundle && oController.getResourceBundle();
        if (!sKey || !oBundle || !oBundle.getText) {
            return "";
        }
        return String(oBundle.getText(sKey) || "");
    }

    return {
        formatSearchModeChipText: function (oController, sMode) {
            var sNorm = String(sMode || "").toUpperCase() === SEARCH_MODE.LOOSE ? SEARCH_MODE.LOOSE : SEARCH_MODE.EXACT;
            var sLabel = resolveBundleText(oController, SearchContracts.SEARCH_MODE_LABEL);
            var sModeText = sNorm === SEARCH_MODE.LOOSE
                ? resolveBundleText(oController, SearchContracts.SEARCH_MODE_LOOSE)
                : resolveBundleText(oController, SearchContracts.SEARCH_MODE_EXACT);
            return sLabel + ": " + sModeText;
        },

        formatWorkflowStageText: function (oController, sStage) {
            return SearchViewStateRuntime.formatWorkflowStageText(
                oController && oController.getResourceBundle && oController.getResourceBundle(),
                sStage
            );
        },

        formatWorkflowStageState: function (sStage) {
            return SearchViewStateRuntime.formatWorkflowStageState(sStage);
        },

        formatSearchResultsCompactText: function (oController, iResultCount, bHasRows) {
            var iSafeCount = Math.max(0, Number(iResultCount || 0));
            var sResultsLabel = resolveBundleText(oController, SearchContracts.RESULTS_LABEL);
            if (!bHasRows || !iSafeCount) {
                return sResultsLabel;
            }
            return sResultsLabel + ": " + iSafeCount;
        },

        formatSearchSelectionSummary: function (oController, iSelectionCount, sSelectedRowDisplayId) {
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
    };
});
