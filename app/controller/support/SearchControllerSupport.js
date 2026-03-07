sap.ui.define([
    "sap_ui5/util/search/SearchMaxResults",
    "sap_ui5/controller/support/SearchSelectionSupport",
    "sap_ui5/controller/support/SearchViewStateSupport"
], function (SearchMaxResults, SearchSelectionSupport, SearchViewStateSupport) {
    "use strict";

    return {
        createViewModel: SearchViewStateSupport.createViewModel,
        normalizeSearchBackendTopValue: SearchMaxResults.normalizeSearchBackendTopValue,
        resolveMaxResults: SearchMaxResults.resolveMaxResults,
        resolveBackendTop: SearchMaxResults.resolveBackendTop,
        resolveSearchUiSessionKey: SearchViewStateSupport.resolveSearchUiSessionKey,
        extractChecklistIdFromObject: SearchSelectionSupport.extractChecklistIdFromObject,
        extractSelectedRowIds: SearchSelectionSupport.extractSelectedRowIds,
        extractSelectedRowId: SearchSelectionSupport.extractSelectedRowId,
        isSmartControlsReady: SearchViewStateSupport.isSmartControlsReady,
        normalizeSearchMaxResultsValue: SearchMaxResults.normalizeSearchMaxResultsValue,
        syncSearchTableRequestWindow: SearchViewStateSupport.syncSearchTableRequestWindow,
        formatHumanDateTime: SearchViewStateSupport.formatHumanDateTime,
        formatWorkflowStageText: SearchViewStateSupport.formatWorkflowStageText,
        formatWorkflowStageState: SearchViewStateSupport.formatWorkflowStageState
    };
});
