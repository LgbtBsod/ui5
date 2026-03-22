sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionTableRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionFocusRuntime"
], function (SearchSelectionStateRuntime, SearchSelectionTableRuntime, SearchSelectionFocusRuntime) {
    "use strict";

    function resolveSearchInnerTable(oController) {
        var oSmartTable = oController.byId("searchSmartTable");
        return oSmartTable && oSmartTable.getTable && oSmartTable.getTable();
    }

    function resolveSmartSearchButton(oController) {
        return oController && oController.byId
            ? oController.byId("searchSmartFilterBar-btnGo")
            : null;
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
        aSelectedRowIds = SearchSelectionStateRuntime.resolveSelectedRowIdsFromInnerTable(oInnerTable);
        return SearchSelectionStateRuntime.selectVisibleRows(
            oController,
            oInnerTable,
            aSelectedRowIds,
            SearchSelectionStateRuntime.resolveSelectedRowDisplayIdFromInnerTable(oInnerTable),
            fnSelectionChanged
        );
    }

    function clearSelection(oController, fnSelectionChanged) {
        var oInnerTable = resolveSearchInnerTable(oController);
        if (oInnerTable && oInnerTable.removeSelections) {
            oInnerTable.removeSelections(true);
        }
        return SearchSelectionStateRuntime.clearSelection(oController, fnSelectionChanged);
    }

    return {
        bindSearchTableRuntime: SearchSelectionTableRuntime.bindSearchTableRuntime,
        clearSelection: clearSelection,
        configureSearchResultTable: SearchSelectionTableRuntime.configureSearchResultTable,
        extractSelectedRowDisplayId: SearchSelectionStateRuntime.extractSelectedRowDisplayId,
        extractSelectedRowId: SearchSelectionStateRuntime.extractSelectedRowId,
        extractSelectedRowIds: SearchSelectionStateRuntime.extractSelectedRowIds,
        focusSearchFilters: function (oController) {
            return SearchSelectionFocusRuntime.focusSearchFilters(oController);
        },
        focusSearchResults: function (oController) {
            return SearchSelectionFocusRuntime.focusSearchResults(oController, resolveSearchInnerTable);
        },
        focusSearchToolbar: function (oController) {
            return SearchSelectionFocusRuntime.focusSearchToolbar(oController);
        },
        resolveSmartSearchButton: resolveSmartSearchButton,
        resolveSearchInnerTable: resolveSearchInnerTable,
        selectVisibleRows: selectVisibleRows,
        syncSearchTableRuntimeState: SearchSelectionTableRuntime.syncSearchTableRuntimeState
    };
});
