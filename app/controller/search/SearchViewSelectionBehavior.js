sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewportRuntime"
], function (SearchCommandPolicy, SearchSelectionRuntime, SearchViewportRuntime) {
    "use strict";

    function dispatchSelectionChanged(oController, mInput) {
        return SearchCommandPolicy.selectionChanged(oController, mInput);
    }

    function clearSelection(oController) {
        return SearchSelectionRuntime.clearSelection(oController, function (mInput) {
            return dispatchSelectionChanged(oController, mInput);
        });
    }

    function selectVisibleRows(oController) {
        return SearchSelectionRuntime.selectVisibleRows(oController, function (mInput) {
            return dispatchSelectionChanged(oController, mInput);
        });
    }

    function selectRowWithScrollCapture(oController, mInput) {
        SearchViewportRuntime.captureSearchScrollPosition(oController);
        return SearchCommandPolicy.selectRow(oController, mInput);
    }

    return {
        clearSelection: clearSelection,
        dispatchSelectionChanged: dispatchSelectionChanged,
        focusSearchResults: SearchSelectionRuntime.focusSearchResults,
        focusSearchToolbar: SearchSelectionRuntime.focusSearchToolbar,
        selectRowWithScrollCapture: selectRowWithScrollCapture,
        selectVisibleRows: selectVisibleRows
    };
});
