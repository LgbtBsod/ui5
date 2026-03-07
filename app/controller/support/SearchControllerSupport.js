sap.ui.define([
    "checklist/app/util/search/SearchMaxResults",
    "checklist/app/controller/support/SearchSelectionSupport",
    "checklist/app/controller/support/SearchViewStateSupport"
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
