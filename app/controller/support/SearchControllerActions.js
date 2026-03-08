sap.ui.define([
    "checklist/app/controller/support/ControllerResourceCleanup",
    "checklist/app/service/domain/search/SearchFacade",
    "checklist/app/service/framework/ControllerRouteRuntime",
    "checklist/app/service/framework/ControllerViewStateRuntime",
    "checklist/app/service/framework/ModelStateRuntime",
    "checklist/app/controller/support/SearchCommandPolicy",
    "checklist/app/controller/support/SearchSelectionSupport",
    "checklist/app/controller/support/SearchLoadRuntimeSupport",
    "checklist/app/controller/support/SearchRateProgress",
    "checklist/app/controller/support/SearchViewSupport",
    "checklist/app/service/framework/SchedulingRuntime",
    "checklist/app/util/search/SearchMaxResults",
    "checklist/app/controller/support/SearchViewStateSupport",
    "sap/ui/core/Item"
], function (ControllerResourceCleanup, SearchFacade, ControllerRouteRuntime, ControllerViewStateRuntime, ModelStateRuntime, SearchCommandPolicy, SearchSelectionSupport, SearchLoadRuntimeSupport, SearchRateProgress, SearchViewSupport, SchedulingRuntime, SearchMaxResults, SearchViewStateSupport, Item) {
    "use strict";

    var DEFAULT_SEARCH_BACKEND_TOP = "100";
    var DEFAULT_SEARCH_VISIBLE_ROWS = "100";

    function normalizeRequestValue(sNormalizedValue, sFallbackValue) {
        var sSafeFallback = String(sFallbackValue || "").trim();
        return String(sNormalizedValue || "").trim() || sSafeFallback || "100";
    }

    function normalizeOptionalRequestValue(sNormalizedValue) {
        return String(sNormalizedValue || "").trim();
    }

    return {
        onInit: function () {
            this._facade = new SearchFacade();
            this._iAnalyticsRefreshTimer = null;
            this._iAnalyticsRailPulseTimer = null;
            this._iSearchWorkingHintTimer = null;
            this._iInitialAnalyticsTimer = null;
            this._iInitialAnalyticsIdleId = null;
            this._iLocationSuggestTimer = null;
            this._aLocationSuggestCache = [];
            this._sLocationSuggestNeedle = "";
            this._searchRateProgress = SearchRateProgress;
            this._sSearchUiSessionKey = SearchViewStateSupport.resolveSearchUiSessionKey();
            this.setModel(SearchViewStateSupport.createViewModel(this._sSearchUiSessionKey), "view");
            ControllerRouteRuntime.attachMatched(this, [
                { name: "search", handler: this._onSearchMatched },
                { name: "detail", handler: this._onDetailSearchContextMatched },
                { name: "detailLayout", handler: this._onDetailSearchContextMatched }
            ]);
            SearchViewSupport.bindAnalyticsRefreshTimer(this);
            SearchViewSupport.syncSmartControlAvailability(this);
            SearchViewSupport.bindPowerUserShortcuts(this);
            SearchViewSupport.bindSearchViewportRuntime(this);
        },

        onExit: function () {
            ControllerRouteRuntime.detachAllMatched(this);
            SearchViewSupport.unbindPowerUserShortcuts(this);
            SearchViewSupport.unbindSearchViewportRuntime(this);
            SearchViewSupport.clearAnalyticsRefreshTimer(this);
            this._iAnalyticsRailPulseTimer = SchedulingRuntime.clearTimer(this._iAnalyticsRailPulseTimer);
            this._iSearchWorkingHintTimer = SchedulingRuntime.clearTimer(this._iSearchWorkingHintTimer);
            this._iInitialAnalyticsTimer = SchedulingRuntime.clearTimer(this._iInitialAnalyticsTimer);
            this._iLocationSuggestTimer = SchedulingRuntime.clearTimer(this._iLocationSuggestTimer);
            if (this._iInitialAnalyticsIdleId && window.cancelIdleCallback) {
                window.cancelIdleCallback(this._iInitialAnalyticsIdleId);
                this._iInitialAnalyticsIdleId = null;
            }
            this._iLocationSuggestTimer = null;
            this._aLocationSuggestCache = [];
            this._sLocationSuggestNeedle = "";
            if (this._oAnalyticsRefreshBinding) {
                this._oAnalyticsRefreshBinding = ControllerResourceCleanup.destroyBinding(this._oAnalyticsRefreshBinding, this._fnAnalyticsRefreshChanged);
            }
            this._fnAnalyticsRefreshChanged = null;
        },

        _withActionBusy: function (sViewBusyPath, fnAction, fnSyncControlBusy) {
            if (typeof fnSyncControlBusy === "function") {
                fnSyncControlBusy(true);
            }
            return ControllerViewStateRuntime.withFlag(this, sViewBusyPath, function () {
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
            ControllerViewStateRuntime.set(this, "/smartFilterReady", true);
            this._bindLocationSuggest();
            SearchCommandPolicy.buildFilter(this, { source: "smartFilterInit" });
        },

        _bindLocationSuggest: function () {
            var oSmartFilterBar = this.byId("searchSmartFilterBar");
            var oLocationControl;
            if (!oSmartFilterBar || typeof oSmartFilterBar.getControlByKey !== "function") {
                return;
            }
            oLocationControl = oSmartFilterBar.getControlByKey("LocationKey");
            if (!oLocationControl || oLocationControl.data("locationSuggestBound")) {
                return;
            }
            if (typeof oLocationControl.setShowSuggestion === "function") {
                oLocationControl.setShowSuggestion(true);
            }
            if (typeof oLocationControl.attachSuggest === "function") {
                oLocationControl.attachSuggest(this.onLocationKeySuggest, this);
            }
            if (typeof oLocationControl.attachSuggestionItemSelected === "function") {
                oLocationControl.attachSuggestionItemSelected(this.onLocationKeySuggestionSelected, this);
            }
            oLocationControl.data("locationSuggestBound", true);
        },

        _updateLocationSuggestions: function (oControl, aItems) {
            if (!oControl || typeof oControl.destroySuggestionItems !== "function" || typeof oControl.addSuggestionItem !== "function") {
                return;
            }
            oControl.destroySuggestionItems();
            (aItems || []).slice(0, 24).forEach(function (oItem) {
                var sCode = String((oItem && (oItem.location_code || oItem.location_id)) || "").trim();
                var sName = String((oItem && oItem.location_name) || "").trim();
                if (!sCode && !sName) {
                    return;
                }
                oControl.addSuggestionItem(new Item({
                    key: sCode,
                    text: sCode,
                    additionalText: sName
                }));
            });
        },

        onLocationKeySuggest: function (oEvent) {
            var sValue = String(oEvent && oEvent.getParameter && (oEvent.getParameter("suggestValue") || oEvent.getParameter("value")) || "").trim();
            var oControl = oEvent && oEvent.getSource && oEvent.getSource();
            var oCtx = this._ctx && this._ctx();
            var oLookup = oCtx && oCtx.locationLookup;
            var sNeedle = sValue.toLowerCase();
            this._iLocationSuggestTimer = SchedulingRuntime.clearTimer(this._iLocationSuggestTimer);
            this._iLocationSuggestTimer = SchedulingRuntime.restartTimer(0, function () {
                this._iLocationSuggestTimer = null;
                if (!oControl) {
                    return;
                }
                if (sNeedle && Array.isArray(this._aLocationSuggestCache) && this._aLocationSuggestCache.length && this._sLocationSuggestNeedle && sNeedle.indexOf(this._sLocationSuggestNeedle) === 0) {
                    this._updateLocationSuggestions(oControl, this._aLocationSuggestCache.filter(function (oItem) {
                        var sCode = String((oItem && (oItem.location_code || oItem.location_id)) || "").toLowerCase();
                        var sName = String((oItem && oItem.location_name) || "").toLowerCase();
                        return sCode.indexOf(sNeedle) >= 0 || sName.indexOf(sNeedle) >= 0;
                    }));
                    return;
                }
                if (!oLookup || typeof oLookup.search !== "function") {
                    this._updateLocationSuggestions(oControl, []);
                    return;
                }
                Promise.resolve(oLookup.search({ query: sValue, limit: 50 }))
                    .then(function (oFound) {
                        var aItems = (oFound && oFound.items) || [];
                        this._aLocationSuggestCache = aItems;
                        this._sLocationSuggestNeedle = sNeedle;
                        this._updateLocationSuggestions(oControl, aItems);
                    }.bind(this))
                    .catch(function () {
                        this._updateLocationSuggestions(oControl, []);
                    }.bind(this));
            }.bind(this), 180);
        },

        onLocationKeySuggestionSelected: function (oEvent) {
            var oSelected = oEvent && oEvent.getParameter && oEvent.getParameter("selectedItem");
            var oControl = oEvent && oEvent.getSource && oEvent.getSource();
            if (!oSelected || !oControl || typeof oControl.setValue !== "function") {
                return;
            }
            oControl.setValue(oSelected.getKey ? oSelected.getKey() : oSelected.getText());
        },

        onSmartFilterChanged: function () {
            var oSmartFilterBar = this.byId("searchSmartFilterBar");
            if (!oSmartFilterBar || (typeof oSmartFilterBar.isInitialised === "function" && !oSmartFilterBar.isInitialised())) {
                return;
            }
            this._bindLocationSuggest();
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
            if (!SearchViewStateSupport.isSmartControlsReady(this)) {
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
            var iSelectionCount = Number(ControllerViewStateRuntime.get(this, "/selectionCount", 0));
            var sSelectedRowId = String(ControllerViewStateRuntime.get(this, "/selectedRowId", "") || "").trim();
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
            var iSelectionCount = Number(ControllerViewStateRuntime.get(this, "/selectionCount", 0));
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
            var sValue = SearchMaxResults.normalizeSearchMaxResultsValue(oEvent && oEvent.getParameter && oEvent.getParameter("value"));
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            sValue = normalizeRequestValue(sValue, ModelStateRuntime.read(this, "state", "/searchMaxResults", DEFAULT_SEARCH_VISIBLE_ROWS));
            ModelStateRuntime.write(this, "state", "/searchMaxResults", sValue);
            if (oSource && typeof oSource.setValue === "function") {
                oSource.setValue(sValue);
            }
            SearchViewStateSupport.syncSearchTableRequestWindow(this);
        },

        onBackendTopChange: function (oEvent) {
            var sValue = SearchMaxResults.normalizeSearchBackendTopValue(oEvent && oEvent.getParameter && oEvent.getParameter("value"));
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            sValue = normalizeOptionalRequestValue(sValue);
            var sCurrentValue = String(ModelStateRuntime.read(this, "state", "/searchBackendTop", DEFAULT_SEARCH_BACKEND_TOP) || "").trim();
            if (sCurrentValue === sValue) {
                if (oSource && typeof oSource.setValue === "function") {
                    oSource.setValue(sValue);
                }
                return;
            }
            ModelStateRuntime.write(this, "state", "/searchBackendTop", sValue);
            if (oSource && typeof oSource.setValue === "function") {
                oSource.setValue(sValue);
            }
            SearchViewStateSupport.syncSearchTableRequestWindow(this);
            if (ControllerViewStateRuntime.get(this, "/hasSearched") &&
                ControllerViewStateRuntime.get(this, "/smartTableReady")) {
                SearchCommandPolicy.rebind(this, { source: "backendTopChange" });
            }
        },

        _syncToolbarRequestInputs: function () {
            var oBackendTopInput = this.byId("backendTopInput");
            var oMaxRowsInput = this.byId("maxRowsInput");
            var sBackendTop = SearchMaxResults.normalizeSearchBackendTopValue(oBackendTopInput && oBackendTopInput.getValue && oBackendTopInput.getValue());
            var sMaxRows = SearchMaxResults.normalizeSearchMaxResultsValue(oMaxRowsInput && oMaxRowsInput.getValue && oMaxRowsInput.getValue());
            sBackendTop = normalizeOptionalRequestValue(sBackendTop);
            sMaxRows = normalizeRequestValue(sMaxRows, ModelStateRuntime.read(this, "state", "/searchMaxResults", DEFAULT_SEARCH_VISIBLE_ROWS));
            ModelStateRuntime.write(this, "state", "/searchBackendTop", sBackendTop);
            ModelStateRuntime.write(this, "state", "/searchMaxResults", sMaxRows);
            if (oBackendTopInput && typeof oBackendTopInput.setValue === "function") {
                oBackendTopInput.setValue(sBackendTop);
            }
            if (oMaxRowsInput && typeof oMaxRowsInput.setValue === "function") {
                oMaxRowsInput.setValue(sMaxRows);
            }
            SearchViewStateSupport.syncSearchTableRequestWindow(this);
        },

        onSearchModeToggle: function (oEvent) {
            var bLoose = !!(oEvent && oEvent.getParameter && oEvent.getParameter("state"));
            ModelStateRuntime.write(this, "state", "/searchMode", bLoose ? "LOOSE" : "EXACT");
            SearchCommandPolicy.executeSearch(this, { intent: "searchModeToggle", state: bLoose });
        },

        onOpenWorkflowAnalytics: function (oEvent) {
            return SearchViewSupport.openWorkflowAnalytics(this);
        },

        formatWorkflowStageText: function (sStage) {
            return SearchViewStateSupport.formatWorkflowStageText(this.getResourceBundle && this.getResourceBundle(), sStage);
        },

        formatWorkflowStageState: function (sStage) {
            return SearchViewStateSupport.formatWorkflowStageState(sStage);
        },

        onSearchTableSelectionChange: function (oEvent) {
            var oSmartTable = this.byId("searchSmartTable");
            var oInnerTable = oSmartTable && oSmartTable.getTable && oSmartTable.getTable();
            var aSelectedRowIds = SearchSelectionSupport.extractSelectedRowIds(oEvent, oInnerTable);
            var sSelectedRowId = aSelectedRowIds[0] || "";
            SearchCommandPolicy.selectionChanged(this, {
                event: oEvent,
                selectedRowId: sSelectedRowId,
                selectedRowIds: aSelectedRowIds,
                source: "tableSelection"
            });
        },

        onSearchTableItemPress: function (oEvent) {
            var sSelectedRowId = SearchSelectionSupport.extractSelectedRowId(oEvent);
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

