sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/ControllerResourceCleanup",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/SearchFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRouteRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchRateProgress",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchViewRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchRuntimeDockFix",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UiDecisionCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/util/search/SearchMaxResults",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchViewStateRuntime",
    "sap/ui/core/Item"
], function (ControllerResourceCleanup, SearchFacade, ControllerRouteRuntime, ControllerViewStateRuntime, ModelStateRuntime, SearchCommandPolicy, SearchSelectionRuntime, SearchLoadRuntime, SearchRateProgress, SearchViewRuntime, SearchRuntimeDockFix, SchedulingRuntime, UiDecisionCoordinator, SearchMaxResults, SearchViewStateRuntime, Item) {
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

    function readAnalyticsDrilldownIntent(oController) {
        return ModelStateRuntime.read(oController, "state", "/analyticsDrilldownIntent", null);
    }

    function clearAnalyticsDrilldownIntent(oController) {
        ModelStateRuntime.write(oController, "state", "/analyticsDrilldownIntent", null);
    }

    function applyFilterValue(oControl, sValue) {
        if (!oControl) {
            return false;
        }
        if (typeof oControl.setSelectedKey === "function") {
            oControl.setSelectedKey(sValue);
        }
        if (typeof oControl.setValue === "function") {
            oControl.setValue(sValue);
        }
        if (typeof oControl.setTokens === "function") {
            oControl.setTokens([]);
        }
        return true;
    }

    function applyAnalyticsDrilldownIntent(oController) {
        var oIntent = readAnalyticsDrilldownIntent(oController) || {};
        var sFilterKey = String(oIntent.filterKey || "").trim();
        var sFilterValue = String(oIntent.filterValue || "").trim();
        var oSmartFilterBar = oController.byId("searchSmartFilterBar");
        var oControl;
        var mFilterData;

        if (!sFilterKey || !sFilterValue || !oSmartFilterBar) {
            return false;
        }
        if (typeof oSmartFilterBar.isInitialised === "function" && !oSmartFilterBar.isInitialised()) {
            return false;
        }
        oControl = typeof oSmartFilterBar.getControlByKey === "function" ? oSmartFilterBar.getControlByKey(sFilterKey) : null;
        applyFilterValue(oControl, sFilterValue);
        if (typeof oSmartFilterBar.getFilterData === "function" && typeof oSmartFilterBar.setFilterData === "function") {
            mFilterData = Object.assign({}, oSmartFilterBar.getFilterData() || {});
            mFilterData[sFilterKey] = sFilterValue;
            oSmartFilterBar.setFilterData(mFilterData, true);
        }
        clearAnalyticsDrilldownIntent(oController);
        SearchCommandPolicy.buildFilter(oController, { source: "analyticsDrilldown" });
        if (ControllerViewStateRuntime.get(oController, "/smartTableReady")) {
            SearchCommandPolicy.rebind(oController, { source: "analyticsDrilldown" });
        }
        return true;
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
            this._sSearchUiSessionKey = SearchViewStateRuntime.resolveSearchUiSessionKey();
            this.setModel(SearchViewStateRuntime.createViewModel(this._sSearchUiSessionKey), "view");
            ControllerRouteRuntime.attachMatched(this, [
                { name: "search", handler: this._onSearchMatched },
                { name: "detail", handler: this._onDetailSearchContextMatched },
                { name: "detailLayout", handler: this._onDetailSearchContextMatched }
            ]);
            SearchViewRuntime.bindAnalyticsRefreshTimer(this);
            SearchViewRuntime.syncSmartControlAvailability(this);
            SearchViewRuntime.bindPowerUserShortcuts(this);
            SearchViewRuntime.bindSearchViewportRuntime(this);
            SearchRuntimeDockFix.bind(this);
        },

        onExit: function () {
            ControllerRouteRuntime.detachAllMatched(this);
            SearchViewRuntime.unbindPowerUserShortcuts(this);
            SearchViewRuntime.unbindSearchViewportRuntime(this);
            SearchViewRuntime.clearAnalyticsRefreshTimer(this);
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
            SearchViewRuntime.syncSmartControlAvailability(this);
        },

        _tryInitialSmartRebind: function () {
            return false;
        },

        _onSearchMatched: function () {
            SearchViewRuntime.onSearchMatched(this);
            SearchRuntimeDockFix.sync(this, true);
            applyAnalyticsDrilldownIntent(this);
        },

        _onDetailSearchContextMatched: function (oEvent) {
            var oArgs = oEvent && oEvent.getParameter && oEvent.getParameter("arguments");
            var sLayout = String((oArgs && oArgs.layout) || "");
            if (sLayout === "MidColumnFullScreen") {
                return;
            }
            SearchViewRuntime.onSearchMatched(this);
            SearchRuntimeDockFix.sync(this, true);
        },

        onSmartFilterInitialise: function () {
            ControllerViewStateRuntime.set(this, "/smartFilterReady", true);
            this._bindLocationSuggest();
            SearchCommandPolicy.buildFilter(this, { source: "smartFilterInit" });
            SearchRuntimeDockFix.bind(this);
            applyAnalyticsDrilldownIntent(this);
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
            SearchViewRuntime.onSmartTableInitialise(this);
            SearchRuntimeDockFix.bind(this);
        },

        onBeforeSmartTableRebind: function (oEvent) {
            this._syncToolbarRequestInputs();
            SearchViewRuntime.onBeforeSmartTableRebind(this, oEvent);
            SearchRuntimeDockFix.sync(this, false);
        },

        onSmartSearch: function () {
            if (!SearchViewStateRuntime.isSmartControlsReady(this)) {
                return Promise.resolve();
            }
            this._syncToolbarRequestInputs();
            SearchLoadRuntime.markLoading(this);
            SearchViewRuntime.beginSearchLoadingFeedback(this);
            return this._withActionBusy("/searchActionBusy", function () {
                return SearchCommandPolicy.executeSearch(this, { source: "smartSearch" });
            }.bind(this), function (bBusy) {
                SearchViewRuntime.setSearchActionBusy(this, bBusy);
            }.bind(this));
        },

        onRetrySearchLoad: function () {
            SearchLoadRuntime.markLoading(this);
            SearchViewRuntime.beginSearchLoadingFeedback(this);
            return SearchCommandPolicy.rebind(this, { source: "searchRetry" }).finally(function () {
                SearchLoadRuntime.setLoadStatus(this, { isLoading: false, isBusy: false, loadError: false });
            }.bind(this)).catch(function (oError) {
                SearchLoadRuntime.applyLoadError(this, String((oError && oError.message) || "Search request failed"));
                return Promise.reject(oError);
            });
        },

        onCreate: function () {
            SearchViewRuntime.captureSearchScrollPosition(this);
            return this._withActionBusy("/createActionBusy", function () {
                return SearchCommandPolicy.selectRow(this, { intent: "create" });
            }.bind(this));
        },

        onOpenSelected: function () {
            var iSelectionCount = Number(ControllerViewStateRuntime.get(this, "/selectionCount", 0));
            var sSelectedRowId = String(ControllerViewStateRuntime.get(this, "/selectedRowId", "") || "").trim();
            return UiDecisionCoordinator.guardOpenSelected({
                controller: this,
                selectionCount: iSelectionCount,
                selectedRowId: sSelectedRowId,
                onMissingSelection: function () {
                    SearchViewRuntime.focusSearchResults(this);
                }.bind(this)
            }).then(function (bAllowed) {
                if (!bAllowed) {
                    return false;
                }
                SearchViewRuntime.captureSearchScrollPosition(this);
                return SearchCommandPolicy.selectRow(this, { intent: "open", rootId: sSelectedRowId, source: "toolbarOpenSelected" });
            }.bind(this));
        },

        onCopy: function () {
            var iSelectionCount = Number(ControllerViewStateRuntime.get(this, "/selectionCount", 0));
            return UiDecisionCoordinator.guardCopySelection({
                controller: this,
                selectionCount: iSelectionCount,
                onBlockedSelection: function () {
                    SearchViewRuntime.focusSearchToolbar(this);
                }.bind(this)
            }).then(function (bAllowed) {
                if (!bAllowed) {
                    return false;
                }
                SearchViewRuntime.captureSearchScrollPosition(this);
                return SearchCommandPolicy.selectRow(this, { intent: "copy" });
            }.bind(this));
        },

        onSelectVisibleRows: function () {
            return SearchViewRuntime.selectVisibleRows(this).then(function (mResult) {
                if (!mResult || !mResult.count) {
                    return UiDecisionCoordinator.notifySelectVisibleEmpty({ controller: this });
                }
                return true;
            }.bind(this));
        },

        onClearSelection: function () {
            return SearchViewRuntime.clearSelection(this).then(function () {
                SearchViewRuntime.focusSearchResults(this);
                return true;
            }.bind(this));
        },

        onScrollSearchAnchor: function () {
            return SearchViewRuntime.scrollToSearchFilters(this);
        },

        onScrollSearchResultsToolbarAnchor: function () {
            return SearchViewRuntime.scrollToSearchResultsToolbar(this);
        },

        onMaxRowsChange: function (oEvent) {
            var sValue = SearchMaxResults.normalizeSearchMaxResultsValue(oEvent && oEvent.getParameter && oEvent.getParameter("value"));
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            sValue = normalizeRequestValue(sValue, ModelStateRuntime.read(this, "state", "/searchMaxResults", DEFAULT_SEARCH_VISIBLE_ROWS));
            ModelStateRuntime.write(this, "state", "/searchMaxResults", sValue);
            if (oSource && typeof oSource.setValue === "function") {
                oSource.setValue(sValue);
            }
            SearchViewStateRuntime.syncSearchTableRequestWindow(this);
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
            SearchViewStateRuntime.syncSearchTableRequestWindow(this);
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
            SearchViewStateRuntime.syncSearchTableRequestWindow(this);
        },

        onSearchModeToggle: function (oEvent) {
            var bLoose = !!(oEvent && oEvent.getParameter && oEvent.getParameter("state"));
            ModelStateRuntime.write(this, "state", "/searchMode", bLoose ? "LOOSE" : "EXACT");
            SearchCommandPolicy.executeSearch(this, { intent: "searchModeToggle", state: bLoose });
        },

        onOpenWorkflowAnalytics: function (oEvent) {
            return SearchViewRuntime.openWorkflowAnalytics(this);
        },

        formatWorkflowStageText: function (sStage) {
            return SearchViewStateRuntime.formatWorkflowStageText(this.getResourceBundle && this.getResourceBundle(), sStage);
        },

        formatWorkflowStageState: function (sStage) {
            return SearchViewStateRuntime.formatWorkflowStageState(sStage);
        },

        onSearchTableSelectionChange: function (oEvent) {
            var oSmartTable = this.byId("searchSmartTable");
            var oInnerTable = oSmartTable && oSmartTable.getTable && oSmartTable.getTable();
            var aSelectedRowIds = SearchSelectionRuntime.extractSelectedRowIds(oEvent, oInnerTable);
            var sSelectedRowId = aSelectedRowIds[0] || "";
            SearchCommandPolicy.selectionChanged(this, {
                event: oEvent,
                selectedRowId: sSelectedRowId,
                selectedRowIds: aSelectedRowIds,
                source: "tableSelection"
            });
        },

        onSearchTableItemPress: function (oEvent) {
            var sSelectedRowId = SearchSelectionRuntime.extractSelectedRowId(oEvent);
            if (!sSelectedRowId) {
                return;
            }
            SearchViewRuntime.captureSearchScrollPosition(this);
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
            return SearchViewRuntime.runExport(this, "screen");
        },

        onExportMenuAction: function (oEvent) {
            var oItem = oEvent.getParameter("item");
            return SearchViewRuntime.runExport(this, oItem && oItem.data("entity") || "screen");
        }
    };
});

