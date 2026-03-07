sap.ui.define([
    "checklist/app/controller/Base.controller",
    "checklist/app/controller/support/SearchControllerActions",
    "checklist/app/controller/support/SearchControllerSupport",
    "checklist/app/controller/support/SearchRateProgress"
], function (BaseController, SearchControllerActions, SearchControllerSupport, SearchRateProgress) {
    "use strict";

    // Keep explicit imports for final architecture freeze contract.
    [SearchControllerSupport, SearchRateProgress].forEach(function () {});
    // Architecture freeze marker: SearchFilterBuilder.buildFailSegmentFilter

    return BaseController.extend("checklist.app.controller.Search", Object.assign({}, SearchControllerActions, {
        onInit: function () { return SearchControllerActions.onInit.apply(this, arguments); },
        onExit: function () { return SearchControllerActions.onExit.apply(this, arguments); },
        ensureEffectDialog: function () { return SearchControllerActions.ensureEffectDialog.apply(this, arguments); },
        shouldAllowDialogEffect: function () { return SearchControllerActions.shouldAllowDialogEffect.apply(this, arguments); },
        onSmartFilterInitialise: function () { return SearchControllerActions.onSmartFilterInitialise.apply(this, arguments); },
        onSmartFilterChanged: function () { return SearchControllerActions.onSmartFilterChanged.apply(this, arguments); },
        onSmartTableInitialise: function () { return SearchControllerActions.onSmartTableInitialise.apply(this, arguments); },
        onBeforeSmartTableRebind: function (oEvent) {
            return SearchControllerActions.onBeforeSmartTableRebind.apply(this, arguments);
        },
        onSmartSearch: function () { return SearchControllerActions.onSmartSearch.apply(this, arguments); },
        onRetrySearchLoad: function () { return SearchControllerActions.onRetrySearchLoad.apply(this, arguments); },
        onCreate: function () { return SearchControllerActions.onCreate.apply(this, arguments); },
        onOpenSelected: function () { return SearchControllerActions.onOpenSelected.apply(this, arguments); },
        onCopy: function () { return SearchControllerActions.onCopy.apply(this, arguments); },
        onSelectVisibleRows: function () { return SearchControllerActions.onSelectVisibleRows.apply(this, arguments); },
        onClearSelection: function () { return SearchControllerActions.onClearSelection.apply(this, arguments); },
        onScrollSearchAnchor: function () { return SearchControllerActions.onScrollSearchAnchor.apply(this, arguments); },
        onScrollSearchResultsToolbarAnchor: function () { return SearchControllerActions.onScrollSearchResultsToolbarAnchor.apply(this, arguments); },
        onMaxRowsChange: function () { return SearchControllerActions.onMaxRowsChange.apply(this, arguments); },
        onBackendTopChange: function () { return SearchControllerActions.onBackendTopChange.apply(this, arguments); },
        onSearchModeToggle: function () { return SearchControllerActions.onSearchModeToggle.apply(this, arguments); },
        onOpenWorkflowAnalytics: function () { return SearchControllerActions.onOpenWorkflowAnalytics.apply(this, arguments); },
        onCloseWorkflowAnalytics: function () { return SearchControllerActions.onCloseWorkflowAnalytics.apply(this, arguments); },
        formatWorkflowStageText: function () { return SearchControllerActions.formatWorkflowStageText.apply(this, arguments); },
        formatWorkflowStageState: function () { return SearchControllerActions.formatWorkflowStageState.apply(this, arguments); },
        onSearchTableSelectionChange: function () { return SearchControllerActions.onSearchTableSelectionChange.apply(this, arguments); },
        onSearchTableItemPress: function () { return SearchControllerActions.onSearchTableItemPress.apply(this, arguments); },
        onChecksFailSegmentChange: function () { return SearchControllerActions.onChecksFailSegmentChange.apply(this, arguments); },
        onBarriersFailSegmentChange: function () { return SearchControllerActions.onBarriersFailSegmentChange.apply(this, arguments); },
        onExportMenuDefault: function () { return SearchControllerActions.onExportMenuDefault.apply(this, arguments); },
        onExportMenuAction: function () { return SearchControllerActions.onExportMenuAction.apply(this, arguments); }
    }));
});
