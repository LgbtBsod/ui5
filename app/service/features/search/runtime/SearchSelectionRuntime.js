sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionIdentityRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionTableRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionFocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionMutationRuntime"
], function (SearchSelectionIdentityRuntime, SearchSelectionTableRuntime, SearchSelectionFocusRuntime, SearchSelectionMutationRuntime) {
    "use strict";

    function resolveSearchInnerTable(oController) {
        var oSmartTable = oController.byId("searchSmartTable");
        return oSmartTable && oSmartTable.getTable && oSmartTable.getTable();
    }

    function selectVisibleRows(oController, fnSelectionChanged) {
        var oInnerTable = resolveSearchInnerTable(oController);
        var aItems = [];
        var aSelectedRowIds = [];
        if (!oInnerTable) {
            return Promise.resolve({ count: 0, selectedRowIds: [] });
        }
        aItems = oInnerTable.getItems ? (oInnerTable.getItems() || []) : [];
        if (oInnerTable.removeSelections) {
            oInnerTable.removeSelections(true);
        }
        if (oInnerTable.selectAll) {
            oInnerTable.selectAll();
        } else if (oInnerTable.setSelectedItem) {
            aItems.forEach(function (oItem) {
                oInnerTable.setSelectedItem(oItem, true);
            });
        }
        aSelectedRowIds = SearchSelectionIdentityRuntime.resolveSelectedRowIdsFromInnerTable(oInnerTable);
        return SearchSelectionMutationRuntime.selectVisibleRows(
            oController,
            oInnerTable,
            aSelectedRowIds,
            SearchSelectionIdentityRuntime.resolveSelectedRowDisplayIdFromInnerTable(oInnerTable),
            fnSelectionChanged
        );
    }

    function clearSelection(oController, fnSelectionChanged) {
        var oInnerTable = resolveSearchInnerTable(oController);
        if (oInnerTable && oInnerTable.removeSelections) {
            oInnerTable.removeSelections(true);
        }
        return SearchSelectionMutationRuntime.clearSelection(oController, fnSelectionChanged);
    }

    return {
        bindSearchTableRuntime: SearchSelectionTableRuntime.bindSearchTableRuntime,
        clearSelection: clearSelection,
        configureSearchResultTable: SearchSelectionTableRuntime.configureSearchResultTable,
        extractSelectedRowDisplayId: SearchSelectionIdentityRuntime.extractSelectedRowDisplayId,
        extractSelectedRowId: SearchSelectionIdentityRuntime.extractSelectedRowId,
        extractSelectedRowIds: SearchSelectionIdentityRuntime.extractSelectedRowIds,
        focusSearchFilters: function (oController) {
            return SearchSelectionFocusRuntime.focusSearchFilters(oController);
        },
        focusSearchResults: function (oController) {
            return SearchSelectionFocusRuntime.focusSearchResults(oController, resolveSearchInnerTable);
        },
        focusSearchToolbar: function (oController) {
            return SearchSelectionFocusRuntime.focusSearchToolbar(oController);
        },
        resolveSearchInnerTable: resolveSearchInnerTable,
        selectVisibleRows: selectVisibleRows,
        syncSearchTableRuntimeState: SearchSelectionTableRuntime.syncSearchTableRuntimeState
    };
});
