sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerActionBusyRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UiDecisionCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchViewBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel"
], function (ControllerActionBusyRuntime, ControllerViewStateRuntime, UiDecisionCoordinator, NavigationIntentService, SearchCommandPolicy, SearchViewBehavior, SearchSelectionRuntime, OperationSourceContracts, CreateSentinel) {
    "use strict";

    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;

    function resolvePressedSearchRowId(oEvent) {
        var oItem = oEvent && oEvent.getParameter && (
            oEvent.getParameter("listItem")
            || oEvent.getParameter("item")
            || oEvent.getSource && oEvent.getSource()
        );
        var oCtx = oItem && oItem.getBindingContext && oItem.getBindingContext();
        var oObject = oCtx && oCtx.getObject && oCtx.getObject();
        return String(
            (oObject && (oObject.Key || oObject.key || oObject.Id || oObject.id || oObject.RequestId || oObject.checklist_id)) || ""
        ).trim();
    }



    function onCreate(oController) {
        SearchViewBehavior.captureSearchScrollPosition(oController);
        return ControllerActionBusyRuntime.withActionBusy(oController, "/createActionBusy", function () {
            NavigationIntentService.navigateToDetail(oController, CreateSentinel.toRouteId());
            return Promise.resolve(true);
        });
    }

    function onCopy(oController) {
        var iSelectionCount = Number(ControllerViewStateRuntime.get(oController, "/selectionCount", 0));
        return UiDecisionCoordinator.guardCopySelection({
            controller: oController,
            selectionCount: iSelectionCount,
            onBlockedSelection: function () {
                SearchViewBehavior.focusSearchToolbar(oController);
            }
        }).then(function (bAllowed) {
            if (!bAllowed) {
                return false;
            }
            SearchViewBehavior.captureSearchScrollPosition(oController);
            return SearchCommandPolicy.selectRow(oController, { intent: SEARCH_SOURCES.COPY });
        });
    }

    function onSelectVisibleRows(oController) {
        return SearchViewBehavior.selectVisibleRows(oController).then(function (mResult) {
            if (!mResult || !mResult.count) {
                return UiDecisionCoordinator.notifySelectVisibleEmpty({ controller: oController });
            }
            return true;
        });
    }

    function onClearSelection(oController) {
        return SearchViewBehavior.clearSelection(oController).then(function () {
            SearchViewBehavior.focusSearchResults(oController);
            return true;
        });
    }

    function onTableSelectionChange(oController, oEvent) {
        var oSmartTable = oController.byId("searchSmartTable");
        var oInnerTable = oSmartTable && oSmartTable.getTable && oSmartTable.getTable();
        var aSelectedRowIds = SearchSelectionRuntime.extractSelectedRowIds(oEvent, oInnerTable);
        var sSelectedRowId = aSelectedRowIds[0] || "";
        var sSelectedRowDisplayId = SearchSelectionRuntime.extractSelectedRowDisplayId(oEvent, oInnerTable);
        SearchCommandPolicy.selectionChanged(oController, {
            event: oEvent,
            selectedRowId: sSelectedRowId,
            selectedRowDisplayId: sSelectedRowDisplayId,
            selectedRowIds: aSelectedRowIds,
            source: SEARCH_SOURCES.TABLE_SELECTION
        });
    }

    function onTableItemPress(oController, oEvent) {
        var sRootId = resolvePressedSearchRowId(oEvent);
        if (!sRootId) {
            return undefined;
        }
        SearchViewBehavior.captureSearchScrollPosition(oController);
        return SearchCommandPolicy.selectRow(oController, {
            intent: SEARCH_SOURCES.OPEN,
            rootId: sRootId,
            source: SEARCH_SOURCES.TABLE_ITEM_PRESS
        });
    }

    function onExportScreen(oController) {
        return SearchViewBehavior.runExport(oController, "screen");
    }

    function onExportMenuAction(oController, oEvent) {
        var oItem = oEvent.getParameter("item");
        return SearchViewBehavior.runExport(oController, oItem && oItem.data("entity") || "screen");
    }

    return {
        onCreate: onCreate,
        onCopy: onCopy,
        onSelectVisibleRows: onSelectVisibleRows,
        onClearSelection: onClearSelection,
        onTableSelectionChange: onTableSelectionChange,
        onTableItemPress: onTableItemPress,
        onExportScreen: onExportScreen,
        onExportMenuAction: onExportMenuAction
    };
});
