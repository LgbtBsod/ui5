sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/UiDecisionDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchActionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchAnalyticsRailRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControlStyleRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadingFeedbackRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/RuntimeOrchestrationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiControlIds",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (
    ControllerViewStateRuntime,
    ModelStateRuntime,
    UiDecisionDefaultHandlers,
    NavigationIntentService,
    SearchCommandPolicy,
    SearchSelectionRuntime,
    SearchActionRuntime,
    SearchAnalyticsRailRuntime,
    SearchViewportRuntime,
    ControlStyleRuntime,
    SearchLoadRuntime,
    SearchLoadingFeedbackRuntime,
    ReadinessTelemetryRuntime,
    OperationSourceContracts,
    ReadinessTelemetryContracts,
    SearchContracts,
    CreateSentinel,
    UiControlIds,
    StatePaths,
    ModelContracts
) {
    "use strict";

    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;
    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var SEARCH_MODE = SearchContracts.SEARCH_MODE;

    function withActionBusy(oController, sPath, fnAction, fnBusyChange) {
        var fnWork = typeof fnAction === "function" ? fnAction : function () { return null; };
        var fnToggle = typeof fnBusyChange === "function" ? fnBusyChange : function () { return null; };

        fnToggle(true);
        return Promise.resolve(ControllerViewStateRuntime.withFlag(oController, sPath, fnWork, true, false)).finally(function () {
            fnToggle(false);
        });
    }

    function dispatchSelectionChanged(oController, mInput) {
        return SearchCommandPolicy.selectionChanged(oController, mInput);
    }

    function focusSearchResults(oController) {
        return SearchSelectionRuntime.focusSearchResults(oController);
    }

    function focusSearchToolbar(oController) {
        return SearchSelectionRuntime.focusSearchToolbar(oController);
    }

    function selectVisibleRows(oController) {
        return SearchSelectionRuntime.selectVisibleRows(oController, function (mInput) {
            return dispatchSelectionChanged(oController, mInput);
        });
    }

    function clearSelection(oController) {
        return SearchSelectionRuntime.clearSelection(oController, function (mInput) {
            return dispatchSelectionChanged(oController, mInput);
        });
    }

    function selectRowWithScrollCapture(oController, mInput) {
        SearchViewportRuntime.captureSearchScrollPosition(oController);
        return SearchCommandPolicy.selectRow(oController, mInput);
    }

    function setSearchActionBusy(oController, bBusy) {
        var oSearchButton = SearchSelectionRuntime.resolveSmartSearchButton(oController);
        if (!oSearchButton) {
            return;
        }
        ControlStyleRuntime.enable(oSearchButton, "searchGoActionBtn");
        if (typeof oSearchButton.setBusy === "function") {
            oSearchButton.setBusy(!!bBusy);
            oSearchButton.setBusyIndicatorDelay(0);
        }
        if (typeof oSearchButton.setEnabled === "function") {
            oSearchButton.setEnabled(!bBusy);
        }
    }

    function onSmartSearch(oController) {
        var bLoose = String(ModelStateRuntime.read(oController, STATE_MODEL, "/search/searchMode", "") || "").trim().toUpperCase() === SEARCH_MODE.LOOSE;
        ReadinessTelemetryRuntime.markControllerStage(oController, ReadinessTelemetryContracts.STAGES.SEARCH_INTERACTION_READY, {
            action: "smartSearch"
        });
        SearchLoadingFeedbackRuntime.beginSearchLoadingFeedback(oController);
        if (!SearchSelectionRuntime.resolveSearchInnerTable(oController) && !ControllerViewStateRuntime.get(oController, "/smartFilterReady", false)) {
            return Promise.resolve();
        }
        SearchLoadRuntime.markLoading(oController);
        return withActionBusy(oController, "/searchActionBusy", function () {
            return SearchCommandPolicy.executeSearch(oController, { source: SEARCH_SOURCES.SMART_SEARCH, state: bLoose });
        }, function (bBusy) {
            setSearchActionBusy(oController, bBusy);
        });
    }

    function onRetrySearchLoad(oController) {
        SearchLoadRuntime.markLoading(oController);
        SearchLoadingFeedbackRuntime.beginSearchLoadingFeedback(oController);
        return SearchCommandPolicy.rebind(oController, { source: SEARCH_SOURCES.SEARCH_RETRY }).finally(function () {
            SearchLoadRuntime.setLoadStatus(oController, { isLoading: false, isBusy: false, loadError: false });
        }).catch(function (oError) {
            SearchLoadRuntime.applyLoadError(oController, String((oError && oError.message) || "Unable to load search results."));
            return Promise.reject(oError);
        });
    }

    function onOpenWorkflowAnalytics(oController) {
        return SearchActionRuntime.openWorkflowAnalytics(oController, {
            navigateToAnalytics: function () {
                NavigationIntentService.navigateToAnalytics(oController);
            }
        });
    }

    function runExport(oController, sEntity) {
        var aSelectedRowIds = ControllerViewStateRuntime.get(oController, "/selectedRowIds", []) || [];
        ControllerViewStateRuntime.set(oController, "/exportBusy", true);
        ModelStateRuntime.write(oController, STATE_MODEL, StatePaths.UI_BUSY_EXPORT, true);
        return Promise.resolve(SearchCommandPolicy.exportFlow(oController, {
            entity: sEntity || "screen",
            selectedRowIds: Array.isArray(aSelectedRowIds) ? aSelectedRowIds.slice(0) : []
        })).finally(function () {
            ControllerViewStateRuntime.set(oController, "/exportBusy", false);
            ModelStateRuntime.write(oController, STATE_MODEL, StatePaths.UI_BUSY_EXPORT, false);
        });
    }

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
        SearchViewportRuntime.captureSearchScrollPosition(oController);
        return withActionBusy(oController, "/createActionBusy", function () {
            NavigationIntentService.navigateToDetail(oController, CreateSentinel.toRouteId());
            return Promise.resolve(true);
        });
    }

    function onCopy(oController) {
        var iSelectionCount = Number(ControllerViewStateRuntime.get(oController, "/selectionCount", 0));
        return Promise.resolve(UiDecisionDefaultHandlers.handlers.guardCopySelection({
            controller: oController,
            selectionCount: iSelectionCount,
            onBlockedSelection: function () {
                focusSearchToolbar(oController);
            }
        })).then(function (bAllowed) {
            if (!bAllowed) {
                return false;
            }
            return selectRowWithScrollCapture(oController, { intent: SEARCH_SOURCES.COPY });
        });
    }

    function onSelectVisibleRows(oController) {
        return selectVisibleRows(oController).then(function (mResult) {
            if (!mResult || !mResult.count) {
                return UiDecisionDefaultHandlers.handlers.notifySelectVisibleEmpty({ controller: oController });
            }
            return true;
        });
    }

    function onClearSelection(oController) {
        return clearSelection(oController).then(function () {
            focusSearchResults(oController);
            return true;
        });
    }

    function onTableSelectionChange(oController, oEvent) {
        var oSmartTable = oController.byId(UiControlIds.SEARCH.SMART_TABLE);
        var oInnerTable = oSmartTable && oSmartTable.getTable && oSmartTable.getTable();
        var aSelectedRowIds = SearchSelectionRuntime.extractSelectedRowIds(oEvent, oInnerTable);
        var sSelectedRowId = aSelectedRowIds[0] || "";
        var sSelectedRowDisplayId = SearchSelectionRuntime.extractSelectedRowDisplayId(oEvent, oInnerTable);
        dispatchSelectionChanged(oController, {
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
        return selectRowWithScrollCapture(oController, {
            intent: SEARCH_SOURCES.OPEN,
            rootId: sRootId,
            source: SEARCH_SOURCES.TABLE_ITEM_PRESS
        });
    }

    function onExportScreen(oController) {
        return runExport(oController, "screen");
    }

    function onExportMenuAction(oController, oEvent) {
        var oItem = oEvent.getParameter("item");
        return runExport(oController, oItem && oItem.data("entity") || "screen");
    }

    return {
        withActionBusy: withActionBusy,
        onSmartSearch: onSmartSearch,
        onRetrySearchLoad: onRetrySearchLoad,
        onCreate: onCreate,
        onCopy: onCopy,
        onSelectVisibleRows: onSelectVisibleRows,
        onClearSelection: onClearSelection,
        onTableSelectionChange: onTableSelectionChange,
        onTableItemPress: onTableItemPress,
        onExportScreen: onExportScreen,
        onExportMenuAction: onExportMenuAction,
        onOpenWorkflowAnalytics: onOpenWorkflowAnalytics
    };
});
