sap.ui.define([
    "checklist/app/controller/support/ControllerResourceCleanup",
    "checklist/app/service/domain/search/SearchFacade",
    "checklist/app/service/framework/CtxFactory",
    "checklist/app/service/framework/FacadeCommandContract",
    "checklist/app/service/framework/FocusRuntime",
    "checklist/app/controller/support/ControllerModelWriteSupport",
    "checklist/app/controller/support/SearchCommandPolicy",
    "checklist/app/controller/support/SearchControllerSupport",
    "checklist/app/controller/support/SearchLoadRuntimeSupport",
    "checklist/app/controller/support/SearchRateProgress",
    "checklist/app/controller/support/SearchViewSupport"
], function (ControllerResourceCleanup, SearchFacade, CtxFactory, FacadeCommandContract, FocusRuntime, ControllerModelWriteSupport, SearchCommandPolicy, SearchControllerSupport, SearchLoadRuntimeSupport, SearchRateProgress, SearchViewSupport) {
    "use strict";

    var DEFAULT_SEARCH_BACKEND_TOP = "100";
    var DEFAULT_SEARCH_VISIBLE_ROWS = "100";

    function normalizeRequestValue(sNormalizedValue, sFallbackValue) {
        var sSafeFallback = String(sFallbackValue || "").trim();
        return String(sNormalizedValue || "").trim() || sSafeFallback || "100";
    }

    return {
        onInit: function () {
            this._facade = new SearchFacade();
            this._mLazyDialogs = {};
            this._iAnalyticsRefreshTimer = null;
            this._iAnalyticsRailPulseTimer = null;
            this._iSearchWorkingHintTimer = null;
            this._iInitialAnalyticsTimer = null;
            this._iInitialAnalyticsIdleId = null;
            this._bWorkflowAnalyticsOpenRequested = false;
            this._oWorkflowAnalyticsReturnFocus = null;
            this._searchRateProgress = SearchRateProgress;
            this._sSearchUiSessionKey = SearchControllerSupport.resolveSearchUiSessionKey();
            this.setModel(SearchControllerSupport.createViewModel(this._sSearchUiSessionKey), "view");
            this.attachRouteMatched("search", this._onSearchMatched);
            this.attachRouteMatched("detail", this._onDetailSearchContextMatched);
            this.attachRouteMatched("detailLayout", this._onDetailSearchContextMatched);
            SearchViewSupport.bindAnalyticsRefreshTimer(this);
            SearchViewSupport.syncSmartControlAvailability(this);
            SearchViewSupport.bindPowerUserShortcuts(this);
            SearchViewSupport.bindSearchViewportRuntime(this);
        },

        onExit: function () {
            if (this.detachAllRouteMatched) {
                this.detachAllRouteMatched();
            }
            SearchViewSupport.unbindPowerUserShortcuts(this);
            SearchViewSupport.unbindSearchViewportRuntime(this);
            SearchViewSupport.clearAnalyticsRefreshTimer(this);
            if (this._iAnalyticsRailPulseTimer) {
                clearTimeout(this._iAnalyticsRailPulseTimer);
                this._iAnalyticsRailPulseTimer = null;
            }
            if (this._iSearchWorkingHintTimer) {
                clearTimeout(this._iSearchWorkingHintTimer);
                this._iSearchWorkingHintTimer = null;
            }
            if (this._iInitialAnalyticsTimer) {
                clearTimeout(this._iInitialAnalyticsTimer);
                this._iInitialAnalyticsTimer = null;
            }
            if (this._iInitialAnalyticsIdleId && window.cancelIdleCallback) {
                window.cancelIdleCallback(this._iInitialAnalyticsIdleId);
                this._iInitialAnalyticsIdleId = null;
            }
            if (this._oAnalyticsRefreshBinding) {
                this._oAnalyticsRefreshBinding = ControllerResourceCleanup.destroyBinding(this._oAnalyticsRefreshBinding, this._fnAnalyticsRefreshChanged);
            }
            this._fnAnalyticsRefreshChanged = null;
            ControllerResourceCleanup.destroyMapEntries(this._mLazyDialogs);
            this._mLazyDialogs = null;
            this._oWorkflowAnalyticsReturnFocus = null;
        },

        _withActionBusy: function (sViewBusyPath, fnAction, fnSyncControlBusy) {
            if (typeof fnSyncControlBusy === "function") {
                fnSyncControlBusy(true);
            }
            return ControllerModelWriteSupport.withFlag(this, "view", sViewBusyPath, function () {
                if (typeof fnAction === "function") {
                    return fnAction();
                }
                return undefined;
            }).finally(function () {
                if (typeof fnSyncControlBusy === "function") {
                    fnSyncControlBusy(false);
                }
            });
        },

        _syncSmartControlAvailability: function () {
            SearchViewSupport.syncSmartControlAvailability(this);
        },

        _tryInitialSmartRebind: function () {
            return false;
        },

        _ctx: function () {
            return CtxFactory.buildCtx(this, {
                smartFilterBar: this.byId("searchSmartFilterBar"),
                smartTable: this.byId("searchSmartTable")
            });
        },

        _execute: function (sMethod, mInput) {
            var sCommand = FacadeCommandContract.normalizeSearchMethod(sMethod);
            var oPayload = FacadeCommandContract.normalizeSearchPayload(sCommand, mInput);
            return this.executeFacadeMethod(this._facade, sCommand, oPayload, this._ctx());
        },

        ensureEffectDialog: function (sId) {
            return SearchViewSupport.ensureEffectDialog(this, sId);
        },

        shouldAllowDialogEffect: function (sId, sAction) {
            return SearchViewSupport.shouldAllowDialogEffect(this, sId, sAction);
        },

        _onSearchMatched: function () {
            SearchViewSupport.onSearchMatched(this);
        },

        _onDetailSearchContextMatched: function (oEvent) {
            var oArgs = oEvent && oEvent.getParameter && oEvent.getParameter("arguments");
            var sLayout = String((oArgs && oArgs.layout) || "");
            if (sLayout === "MidColumnFullScreen") {
                return;
            }
            SearchViewSupport.onSearchMatched(this);
        },

        onSmartFilterInitialise: function () {
            ControllerModelWriteSupport.set(this, "view", "/smartFilterReady", true);
            SearchCommandPolicy.buildFilter(this, { source: "smartFilterInit" });
        },

        onSmartFilterChanged: function () {
            var oSmartFilterBar = this.byId("searchSmartFilterBar");
            if (!oSmartFilterBar || (typeof oSmartFilterBar.isInitialised === "function" && !oSmartFilterBar.isInitialised())) {
                return;
            }
            SearchCommandPolicy.buildFilter(this, { source: "smartFilterChanged" });
        },

        onSmartTableInitialise: function () {
            SearchViewSupport.onSmartTableInitialise(this);
        },

        onBeforeSmartTableRebind: function (oEvent) {
            this._syncToolbarRequestInputs();
            SearchViewSupport.onBeforeSmartTableRebind(this, oEvent);
        },

        onSmartSearch: function () {
            if (!SearchControllerSupport.isSmartControlsReady(this)) {
                return Promise.resolve();
            }
            this._syncToolbarRequestInputs();
            SearchLoadRuntimeSupport.markLoading(this);
            SearchViewSupport.beginSearchLoadingFeedback(this);
            return this._withActionBusy("/searchActionBusy", function () {
                return SearchCommandPolicy.executeSearch(this, { source: "smartSearch" });
            }.bind(this), function (bBusy) {
                SearchViewSupport.setSearchActionBusy(this, bBusy);
            }.bind(this));
        },

        onRetrySearchLoad: function () {
            var oStateModel = this.getModel("state");
            if (oStateModel && oStateModel.getProperty && oStateModel.getProperty("/networkOnline") === false) {
                this.showI18nError("searchOfflineMessage");
                return Promise.resolve(false);
            }
            SearchLoadRuntimeSupport.markLoading(this);
            SearchViewSupport.beginSearchLoadingFeedback(this);
            return SearchCommandPolicy.rebind(this, { source: "searchRetry" }).finally(function () {
                SearchLoadRuntimeSupport.setLoadStatus(this, { isLoading: false, isBusy: false, loadError: false });
            }.bind(this)).catch(function (oError) {
                SearchLoadRuntimeSupport.applyLoadError(this, String((oError && oError.message) || "Search request failed"));
                return Promise.reject(oError);
            });
        },

        onCreate: function () {
            SearchViewSupport.captureSearchScrollPosition(this);
            return this._withActionBusy("/createActionBusy", function () {
                return SearchCommandPolicy.selectRow(this, { intent: "create" });
            }.bind(this));
        },

        onOpenSelected: function () {
            var iSelectionCount = Number(ControllerModelWriteSupport.get(this, "view", "/selectionCount", 0));
            var sSelectedRowId = String(ControllerModelWriteSupport.get(this, "view", "/selectedRowId", "") || "").trim();
            if (!sSelectedRowId) {
                this.showI18nError("nothingToOpen");
                SearchViewSupport.focusSearchResults(this);
                return Promise.resolve(false);
            }
            if (iSelectionCount > 1) {
                this.showI18nToast("searchOpenUsesFirstHint", [iSelectionCount]);
            }
            SearchViewSupport.captureSearchScrollPosition(this);
            return SearchCommandPolicy.selectRow(this, { intent: "open", rootId: sSelectedRowId, source: "toolbarOpenSelected" });
        },

        onCopy: function () {
            var iSelectionCount = Number(ControllerModelWriteSupport.get(this, "view", "/selectionCount", 0));
            if (iSelectionCount > 1) {
                this.showI18nError("searchCopySingleSelectionHint");
                SearchViewSupport.focusSearchToolbar(this);
                return Promise.resolve(false);
            }
            SearchViewSupport.captureSearchScrollPosition(this);
            return SearchCommandPolicy.selectRow(this, { intent: "copy" });
        },

        onSelectVisibleRows: function () {
            return SearchViewSupport.selectVisibleRows(this).then(function (mResult) {
                if (!mResult || !mResult.count) {
                    this.showI18nError("searchSelectVisibleEmpty");
                    return false;
                }
                return true;
            }.bind(this));
        },

        onClearSelection: function () {
            return SearchViewSupport.clearSelection(this).then(function () {
                SearchViewSupport.focusSearchResults(this);
                return true;
            }.bind(this));
        },

        onScrollSearchAnchor: function () {
            return SearchViewSupport.scrollToSearchFilters(this);
        },

        onScrollSearchResultsToolbarAnchor: function () {
            return SearchViewSupport.scrollToSearchResultsToolbar(this);
        },

        onMaxRowsChange: function (oEvent) {
            var sValue = SearchControllerSupport.normalizeSearchMaxResultsValue(oEvent && oEvent.getParameter && oEvent.getParameter("value"));
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            sValue = normalizeRequestValue(sValue, ControllerModelWriteSupport.get(this, "state", "/searchMaxResults", DEFAULT_SEARCH_VISIBLE_ROWS));
            ControllerModelWriteSupport.set(this, "state", "/searchMaxResults", sValue);
            if (oSource && typeof oSource.setValue === "function") {
                oSource.setValue(sValue);
            }
            SearchControllerSupport.syncSearchTableRequestWindow(this);
        },

        onBackendTopChange: function (oEvent) {
            var sValue = SearchControllerSupport.normalizeSearchBackendTopValue(oEvent && oEvent.getParameter && oEvent.getParameter("value"));
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            sValue = normalizeRequestValue(sValue, ControllerModelWriteSupport.get(this, "state", "/searchBackendTop", DEFAULT_SEARCH_BACKEND_TOP));
            ControllerModelWriteSupport.set(this, "state", "/searchBackendTop", sValue);
            if (oSource && typeof oSource.setValue === "function") {
                oSource.setValue(sValue);
            }
            SearchControllerSupport.syncSearchTableRequestWindow(this);
            if (ControllerModelWriteSupport.get(this, "view", "/hasSearched") &&
                ControllerModelWriteSupport.get(this, "view", "/smartTableReady")) {
                SearchCommandPolicy.rebind(this, { source: "backendTopChange" });
            }
        },

        _syncToolbarRequestInputs: function () {
            var oBackendTopInput = this.byId("backendTopInput");
            var oMaxRowsInput = this.byId("maxRowsInput");
            var sBackendTop = SearchControllerSupport.normalizeSearchBackendTopValue(oBackendTopInput && oBackendTopInput.getValue && oBackendTopInput.getValue());
            var sMaxRows = SearchControllerSupport.normalizeSearchMaxResultsValue(oMaxRowsInput && oMaxRowsInput.getValue && oMaxRowsInput.getValue());
            sBackendTop = normalizeRequestValue(sBackendTop, ControllerModelWriteSupport.get(this, "state", "/searchBackendTop", DEFAULT_SEARCH_BACKEND_TOP));
            sMaxRows = normalizeRequestValue(sMaxRows, ControllerModelWriteSupport.get(this, "state", "/searchMaxResults", DEFAULT_SEARCH_VISIBLE_ROWS));
            ControllerModelWriteSupport.set(this, "state", "/searchBackendTop", sBackendTop);
            ControllerModelWriteSupport.set(this, "state", "/searchMaxResults", sMaxRows);
            if (oBackendTopInput && typeof oBackendTopInput.setValue === "function") {
                oBackendTopInput.setValue(sBackendTop);
            }
            if (oMaxRowsInput && typeof oMaxRowsInput.setValue === "function") {
                oMaxRowsInput.setValue(sMaxRows);
            }
            SearchControllerSupport.syncSearchTableRequestWindow(this);
        },

        onSearchModeToggle: function (oEvent) {
            var bLoose = !!(oEvent && oEvent.getParameter && oEvent.getParameter("state"));
            ControllerModelWriteSupport.set(this, "state", "/searchMode", bLoose ? "LOOSE" : "EXACT");
            SearchCommandPolicy.executeSearch(this, { intent: "searchModeToggle", state: bLoose });
        },

        onOpenWorkflowAnalytics: function (oEvent) {
            var oSource = (oEvent && oEvent.getParameter && oEvent.getParameter("anchor"))
                || (oEvent && oEvent.getSource && oEvent.getSource())
                || (oEvent && oEvent.source)
                || null;
            if (oSource) {
                this._oWorkflowAnalyticsReturnFocus = oSource;
            }
            return SearchViewSupport.openWorkflowAnalytics(this);
        },

        onCloseWorkflowAnalytics: function () {
            SearchViewSupport.closeWorkflowAnalytics(this);
        },

        _restoreWorkflowAnalyticsFocus: function () {
            var oFocusTarget = this._oWorkflowAnalyticsReturnFocus;
            FocusRuntime.focusSoon(oFocusTarget);
            this._oWorkflowAnalyticsReturnFocus = null;
        },

        formatWorkflowStageText: function (sStage) {
            return SearchControllerSupport.formatWorkflowStageText(this.getResourceBundle && this.getResourceBundle(), sStage);
        },

        formatWorkflowStageState: function (sStage) {
            return SearchControllerSupport.formatWorkflowStageState(sStage);
        },

        onSearchTableSelectionChange: function (oEvent) {
            var oSmartTable = this.byId("searchSmartTable");
            var oInnerTable = oSmartTable && oSmartTable.getTable && oSmartTable.getTable();
            var aSelectedRowIds = SearchControllerSupport.extractSelectedRowIds(oEvent, oInnerTable);
            var sSelectedRowId = aSelectedRowIds[0] || "";
            SearchCommandPolicy.selectionChanged(this, {
                event: oEvent,
                selectedRowId: sSelectedRowId,
                selectedRowIds: aSelectedRowIds,
                source: "tableSelection"
            });
        },

        onSearchTableItemPress: function (oEvent) {
            var sSelectedRowId = SearchControllerSupport.extractSelectedRowId(oEvent);
            if (!sSelectedRowId) {
                return;
            }
            SearchViewSupport.captureSearchScrollPosition(this);
            SearchCommandPolicy.selectionChanged(this, {
                selectedRowId: sSelectedRowId,
                selectedRowIds: [sSelectedRowId],
                source: "tableItemPress"
            });
            SearchCommandPolicy.selectRow(this, { intent: "open", rootId: sSelectedRowId, source: "tableItemPress" });
        },

        onChecksFailSegmentChange: function (oEvent) {
            SearchCommandPolicy.buildFilter(this, { intent: "checksSegment", key: oEvent.getParameter("key") });
        },

        onBarriersFailSegmentChange: function (oEvent) {
            SearchCommandPolicy.buildFilter(this, { intent: "barriersSegment", key: oEvent.getParameter("key") });
        },

        _legacySmartTableContractHint: function () {
            return "SearchFilterBuilder.buildFailSegmentFilter";
        },

        onExportMenuDefault: function () {
            return SearchViewSupport.runExport(this, "screen");
        },

        onExportMenuAction: function (oEvent) {
            var oItem = oEvent.getParameter("item");
            return SearchViewSupport.runExport(this, oItem && oItem.data("entity") || "screen");
        }
    };
});

