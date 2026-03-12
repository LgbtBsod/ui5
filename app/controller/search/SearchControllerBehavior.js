sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/shared/ControllerResourceCleanup",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/SearchFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRouteRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchRateProgress",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchViewBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UiDecisionCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/util/search/SearchMaxResults",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/SearchRuntimeContracts",
    "sap/ui/core/Item",
    "sap/m/ViewSettingsDialog",
    "sap/m/ViewSettingsItem",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/NavigationContracts"
], function (ControllerResourceCleanup, SearchFacade, ControllerRouteRuntime, ControllerViewStateRuntime, ModelStateRuntime, SearchCommandPolicy, SearchSelectionRuntime, SearchLoadRuntime, SearchRateProgress, SearchViewBehavior, SchedulingRuntime, UiDecisionCoordinator, NavigationIntentService, CreateSentinel, SearchMaxResults, SearchViewStateRuntime, ModelContracts, OperationSourceContracts, SearchRuntimeContracts, Item, ViewSettingsDialog, ViewSettingsItem, NavigationContracts) {
    "use strict";

    var DEFAULT_SEARCH_BACKEND_TOP = SearchRuntimeContracts.DEFAULTS.SEARCH_BACKEND_TOP;
    var DEFAULT_SEARCH_VISIBLE_ROWS = SearchRuntimeContracts.DEFAULTS.SEARCH_VISIBLE_ROWS;
    var SEARCH_MODE = SearchRuntimeContracts.SEARCH_MODE;
    var MODELS = ModelContracts.MODELS;
    var TOKENS = ModelContracts.TOKENS;
    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;
    var STATE_MODEL = MODELS.STATE;
    var VIEW_MODEL = MODELS.VIEW;
    var ANALYTICS_DRILLDOWN_INTENT_PATH = "/analyticsDrilldownIntent";
    var SEARCH_SORT_KEY_PATH = "/searchSortKey";
    var SEARCH_SORT_DESCENDING_PATH = "/searchSortDescending";
    var SEARCH_GROUP_KEY_PATH = "/searchGroupKey";
    var SEARCH_GROUP_DESCENDING_PATH = "/searchGroupDescending";

    function normalizeRequestValue(sNormalizedValue, sFallbackValue) {
        var sSafeFallback = String(sFallbackValue || "").trim();
        return String(sNormalizedValue || "").trim() || sSafeFallback || "100";
    }

    function normalizeOptionalRequestValue(sNormalizedValue) {
        return String(sNormalizedValue || "").trim();
    }

    function readAnalyticsDrilldownIntent(oController) {
        return ModelStateRuntime.read(oController, STATE_MODEL, ANALYTICS_DRILLDOWN_INTENT_PATH, null);
    }

    function clearAnalyticsDrilldownIntent(oController) {
        ModelStateRuntime.write(oController, STATE_MODEL, ANALYTICS_DRILLDOWN_INTENT_PATH, null);
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

    function resolveSortItems(oController) {
        var oBundle = oController.getResourceBundle && oController.getResourceBundle();
        return [
            { key: "DateCheck", text: oBundle && oBundle.getText("searchSortDateCheck") || "Date" },
            { key: "Id", text: oBundle && oBundle.getText("searchSortId") || "ID" },
            { key: "Status", text: oBundle && oBundle.getText("searchSortStatus") || "Status" },
            { key: "LpcText", text: oBundle && oBundle.getText("searchSortLpc") || "LPC" },
            { key: "ProfessionText", text: oBundle && oBundle.getText("searchSortProfession") || "Profession" },
            { key: "ChangedOn", text: oBundle && oBundle.getText("searchSortChangedOn") || "Changed on" }
        ];
    }

    function resolveGroupItems(oController) {
        var oBundle = oController.getResourceBundle && oController.getResourceBundle();
        return [
            { key: "__NONE__", text: oBundle && oBundle.getText("searchGroupNone") || "No grouping" },
            { key: "Status", text: oBundle && oBundle.getText("searchGroupStatus") || "Status" },
            { key: "LpcText", text: oBundle && oBundle.getText("searchGroupLpc") || "LPC" },
            { key: "ProfessionText", text: oBundle && oBundle.getText("searchGroupProfession") || "Profession" },
            { key: "DateCheck", text: oBundle && oBundle.getText("searchGroupDateCheck") || "Date" }
        ];
    }

    function shouldRebindSearch(oController) {
        return !!(ControllerViewStateRuntime.get(oController, "/hasSearched")
            && ControllerViewStateRuntime.get(oController, "/smartTableReady"));
    }

    function applySearchSortSettings(oController, mSettings) {
        var sSortKey = String((mSettings && mSettings.sortKey) || "").trim() || TOKENS.DATE_CHECK;
        var bSortDescending = !!(mSettings && mSettings.sortDescending);
        ModelStateRuntime.write(oController, STATE_MODEL, SEARCH_SORT_KEY_PATH, sSortKey);
        ModelStateRuntime.write(oController, STATE_MODEL, SEARCH_SORT_DESCENDING_PATH, bSortDescending);
        if (shouldRebindSearch(oController)) {
            SearchCommandPolicy.rebind(oController, { source: SEARCH_SOURCES.SEARCH_SORT_SETTINGS });
        }
    }

    function applySearchGroupSettings(oController, mSettings) {
        var sGroupKey = String((mSettings && mSettings.groupKey) || "").trim();
        var bGroupDescending = !!(mSettings && mSettings.groupDescending);
        if (sGroupKey === TOKENS.GROUP_NONE) {
            sGroupKey = "";
        }
        ModelStateRuntime.write(oController, STATE_MODEL, SEARCH_GROUP_KEY_PATH, sGroupKey);
        ModelStateRuntime.write(oController, STATE_MODEL, SEARCH_GROUP_DESCENDING_PATH, bGroupDescending);
        if (shouldRebindSearch(oController)) {
            SearchCommandPolicy.rebind(oController, { source: SEARCH_SOURCES.SEARCH_GROUP_SETTINGS });
        }
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
        SearchCommandPolicy.buildFilter(oController, { source: SEARCH_SOURCES.ANALYTICS_DRILLDOWN });
        if (ControllerViewStateRuntime.get(oController, "/smartTableReady")) {
            SearchCommandPolicy.rebind(oController, { source: SEARCH_SOURCES.ANALYTICS_DRILLDOWN });
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
            this.setModel(SearchViewStateRuntime.createViewModel(this._sSearchUiSessionKey), VIEW_MODEL);
            if (!String(ModelStateRuntime.read(this, STATE_MODEL, SEARCH_SORT_KEY_PATH, "")).trim()) {
                ModelStateRuntime.write(this, STATE_MODEL, SEARCH_SORT_KEY_PATH, TOKENS.DATE_CHECK);
            }
            if (typeof ModelStateRuntime.read(this, STATE_MODEL, SEARCH_SORT_DESCENDING_PATH, undefined) !== "boolean") {
                ModelStateRuntime.write(this, STATE_MODEL, SEARCH_SORT_DESCENDING_PATH, true);
            }
            if (!String(ModelStateRuntime.read(this, STATE_MODEL, SEARCH_GROUP_KEY_PATH, "")).trim()) {
                ModelStateRuntime.write(this, STATE_MODEL, SEARCH_GROUP_KEY_PATH, "");
            }
            if (typeof ModelStateRuntime.read(this, STATE_MODEL, SEARCH_GROUP_DESCENDING_PATH, undefined) !== "boolean") {
                ModelStateRuntime.write(this, STATE_MODEL, SEARCH_GROUP_DESCENDING_PATH, false);
            }
            ControllerRouteRuntime.attachMatched(this, [
                { name: NavigationContracts.ROUTES.SEARCH, handler: this._onSearchMatched },
                { name: NavigationContracts.ROUTES.DETAIL, handler: this._onDetailSearchContextMatched },
                { name: NavigationContracts.ROUTES.DETAIL_LAYOUT, handler: this._onDetailSearchContextMatched }
            ]);
            SearchViewBehavior.bindAnalyticsRefreshTimer(this);
            SearchViewBehavior.syncSmartControlAvailability(this);
            SearchViewBehavior.bindPowerUserShortcuts(this);
            SearchViewBehavior.bindSearchViewportRuntime(this);
        },

        onExit: function () {
            ControllerRouteRuntime.detachAllMatched(this);
            SearchViewBehavior.unbindPowerUserShortcuts(this);
            SearchViewBehavior.unbindSearchViewportRuntime(this);
            SearchViewBehavior.clearAnalyticsRefreshTimer(this);
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
            if (this._oSearchSortDialog) {
                this._oSearchSortDialog.destroy();
                this._oSearchSortDialog = null;
            }
            if (this._oSearchGroupDialog) {
                this._oSearchGroupDialog.destroy();
                this._oSearchGroupDialog = null;
            }
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
            SearchViewBehavior.syncSmartControlAvailability(this);
        },

        _tryInitialSmartRebind: function () {
            return false;
        },

        _onSearchMatched: function () {
            SearchViewBehavior.onSearchMatched(this);
            applyAnalyticsDrilldownIntent(this);
        },

        _onDetailSearchContextMatched: function (oEvent) {
            var oArgs = oEvent && oEvent.getParameter && oEvent.getParameter("arguments");
            var sLayout = String((oArgs && oArgs.layout) || "");
            if (sLayout === NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN) {
                return;
            }
            SearchViewBehavior.syncSearchContextForDetailRoute(this);
        },

        onSmartFilterInitialise: function () {
            ControllerViewStateRuntime.set(this, "/smartFilterReady", true);
            this._bindLocationSuggest();
            SearchCommandPolicy.buildFilter(this, { source: SEARCH_SOURCES.SMART_FILTER_INIT });
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
            SearchCommandPolicy.buildFilter(this, { source: SEARCH_SOURCES.SMART_FILTER_CHANGED });
        },

        onSmartTableInitialise: function () {
            SearchViewBehavior.onSmartTableInitialise(this);
        },

        onBeforeSmartTableRebind: function (oEvent) {
            this._syncToolbarRequestInputs();
            SearchViewBehavior.onBeforeSmartTableRebind(this, oEvent);
        },

        onSmartSearch: function () {
            if (!SearchViewStateRuntime.isSmartControlsReady(this)) {
                return Promise.resolve();
            }
            this._syncToolbarRequestInputs();
            SearchLoadRuntime.markLoading(this);
            SearchViewBehavior.beginSearchLoadingFeedback(this);
            return this._withActionBusy("/searchActionBusy", function () {
                return SearchCommandPolicy.executeSearch(this, { source: SEARCH_SOURCES.SMART_SEARCH });
            }.bind(this), function (bBusy) {
                SearchViewBehavior.setSearchActionBusy(this, bBusy);
            }.bind(this));
        },

        onRetrySearchLoad: function () {
            SearchLoadRuntime.markLoading(this);
            SearchViewBehavior.beginSearchLoadingFeedback(this);
            return SearchCommandPolicy.rebind(this, { source: SEARCH_SOURCES.SEARCH_RETRY }).finally(function () {
                SearchLoadRuntime.setLoadStatus(this, { isLoading: false, isBusy: false, loadError: false });
            }.bind(this)).catch(function (oError) {
                SearchLoadRuntime.applyLoadError(this, String((oError && oError.message) || "Search request failed"));
                return Promise.reject(oError);
            });
        },

        onCreate: function () {
            SearchViewBehavior.captureSearchScrollPosition(this);
            return this._withActionBusy("/createActionBusy", function () {
                NavigationIntentService.navigateToDetail(this, CreateSentinel.toRouteId());
                return Promise.resolve(true);
            }.bind(this));
        },

        onCopy: function () {
            var iSelectionCount = Number(ControllerViewStateRuntime.get(this, "/selectionCount", 0));
            return UiDecisionCoordinator.guardCopySelection({
                controller: this,
                selectionCount: iSelectionCount,
                onBlockedSelection: function () {
                    SearchViewBehavior.focusSearchToolbar(this);
                }.bind(this)
            }).then(function (bAllowed) {
                if (!bAllowed) {
                    return false;
                }
                SearchViewBehavior.captureSearchScrollPosition(this);
                return SearchCommandPolicy.selectRow(this, { intent: SEARCH_SOURCES.COPY });
            }.bind(this));
        },

        onSelectVisibleRows: function () {
            return SearchViewBehavior.selectVisibleRows(this).then(function (mResult) {
                if (!mResult || !mResult.count) {
                    return UiDecisionCoordinator.notifySelectVisibleEmpty({ controller: this });
                }
                return true;
            }.bind(this));
        },

        onClearSelection: function () {
            return SearchViewBehavior.clearSelection(this).then(function () {
                SearchViewBehavior.focusSearchResults(this);
                return true;
            }.bind(this));
        },

        onScrollSearchAnchor: function () {
            return SearchViewBehavior.scrollToSearchFilters(this);
        },

        onScrollSearchResultsToolbarAnchor: function () {
            return SearchViewBehavior.scrollToSearchResultsToolbar(this);
        },

        onMaxRowsChange: function (oEvent) {
            var sValue = SearchMaxResults.normalizeSearchMaxResultsValue(oEvent && oEvent.getParameter && oEvent.getParameter("value"));
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            sValue = normalizeRequestValue(sValue, ModelStateRuntime.read(this, STATE_MODEL, "/searchMaxResults", DEFAULT_SEARCH_VISIBLE_ROWS));
            ModelStateRuntime.write(this, STATE_MODEL, "/searchMaxResults", sValue);
            ModelStateRuntime.write(this, STATE_MODEL, "/growingPageSize", sValue);
            if (oSource && typeof oSource.setValue === "function") {
                oSource.setValue(sValue);
            }
            SearchViewStateRuntime.syncSearchTableRequestWindow(this);
        },

        onBackendTopChange: function (oEvent) {
            var sValue = SearchMaxResults.normalizeSearchBackendTopValue(oEvent && oEvent.getParameter && oEvent.getParameter("value"));
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            sValue = normalizeOptionalRequestValue(sValue);
            var sCurrentValue = String(ModelStateRuntime.read(this, STATE_MODEL, "/searchBackendTop", DEFAULT_SEARCH_BACKEND_TOP) || "").trim();
            if (sCurrentValue === sValue) {
                if (oSource && typeof oSource.setValue === "function") {
                    oSource.setValue(sValue);
                }
                return;
            }
            ModelStateRuntime.write(this, STATE_MODEL, "/searchBackendTop", sValue);
            ModelStateRuntime.write(this, STATE_MODEL, "/searchFetchLimit", sValue);
            if (oSource && typeof oSource.setValue === "function") {
                oSource.setValue(sValue);
            }
            SearchViewStateRuntime.syncSearchTableRequestWindow(this);
            if (ControllerViewStateRuntime.get(this, "/hasSearched") &&
                ControllerViewStateRuntime.get(this, "/smartTableReady")) {
                SearchCommandPolicy.rebind(this, { source: SEARCH_SOURCES.BACKEND_TOP_CHANGE });
            }
        },

        _syncToolbarRequestInputs: function () {
            var oBackendTopInput = this.byId("backendTopInput");
            var oMaxRowsInput = this.byId("maxRowsInput");
            var sBackendTop = SearchMaxResults.normalizeSearchBackendTopValue(oBackendTopInput && oBackendTopInput.getValue && oBackendTopInput.getValue());
            var sMaxRows = SearchMaxResults.normalizeSearchMaxResultsValue(oMaxRowsInput && oMaxRowsInput.getValue && oMaxRowsInput.getValue());
            sBackendTop = normalizeOptionalRequestValue(sBackendTop);
            sMaxRows = normalizeRequestValue(sMaxRows, ModelStateRuntime.read(this, STATE_MODEL, "/searchMaxResults", DEFAULT_SEARCH_VISIBLE_ROWS));
            ModelStateRuntime.write(this, STATE_MODEL, "/searchBackendTop", sBackendTop);
            ModelStateRuntime.write(this, STATE_MODEL, "/searchFetchLimit", sBackendTop);
            ModelStateRuntime.write(this, STATE_MODEL, "/searchMaxResults", sMaxRows);
            ModelStateRuntime.write(this, STATE_MODEL, "/growingPageSize", sMaxRows);
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
            ModelStateRuntime.write(this, STATE_MODEL, "/searchMode", bLoose ? SEARCH_MODE.LOOSE : SEARCH_MODE.EXACT);
            SearchCommandPolicy.executeSearch(this, { intent: SEARCH_SOURCES.SEARCH_MODE_TOGGLE, state: bLoose });
        },

        formatSearchModeChipText: function (sMode) {
            var oBundle = this.getResourceBundle && this.getResourceBundle();
            var sNorm = String(sMode || "").toUpperCase() === SEARCH_MODE.LOOSE ? SEARCH_MODE.LOOSE : SEARCH_MODE.EXACT;
            var sLabel = oBundle && oBundle.getText("searchModeLabel") || "Mode";
            var sModeText = sNorm === "LOOSE"
                ? (oBundle && oBundle.getText("searchModeLoose") || "Loose")
                : (oBundle && oBundle.getText("searchModeExact") || "Exact");
            return sLabel + ": " + sModeText;
        },

        onOpenSearchSortDialog: function () {
            var sSelectedKey = String(ModelStateRuntime.read(this, STATE_MODEL, "/searchSortKey", TOKENS.DATE_CHECK) || TOKENS.DATE_CHECK);
            var bSelectedDescending = !!ModelStateRuntime.read(this, STATE_MODEL, "/searchSortDescending", true);
            var oBundle = this.getResourceBundle && this.getResourceBundle();
            if (this._oSearchGroupDialog && this._oSearchGroupDialog.isOpen && this._oSearchGroupDialog.isOpen()) {
                this._oSearchGroupDialog.close();
            }
            if (!this._oSearchSortDialog) {
                this._oSearchSortDialog = new ViewSettingsDialog({
                    title: oBundle && oBundle.getText("searchSortDialogTitle") || "Sort",
                    confirm: this.onSearchSortDialogConfirm.bind(this)
                });
                resolveSortItems(this).forEach(function (oItem) {
                    this._oSearchSortDialog.addSortItem(new ViewSettingsItem({ key: oItem.key, text: oItem.text }));
                }.bind(this));
                this.getView().addDependent(this._oSearchSortDialog);
            }
            this._oSearchSortDialog.setSelectedSortItem(sSelectedKey);
            this._oSearchSortDialog.setSortDescending(bSelectedDescending);
            this._oSearchSortDialog.open("sort");
        },

        onSearchSortDialogConfirm: function (oEvent) {
            var oSortItem = oEvent && oEvent.getParameter && oEvent.getParameter("sortItem");
            var bSortDescending = !!(oEvent && oEvent.getParameter && oEvent.getParameter("sortDescending"));
            applySearchSortSettings(this, {
                sortKey: oSortItem && oSortItem.getKey && oSortItem.getKey(),
                sortDescending: bSortDescending
            });
        },

        onOpenSearchGroupDialog: function () {
            var sSelectedKey = String(ModelStateRuntime.read(this, STATE_MODEL, "/searchGroupKey", "") || TOKENS.GROUP_NONE);
            var bSelectedDescending = !!ModelStateRuntime.read(this, STATE_MODEL, "/searchGroupDescending", false);
            var oBundle = this.getResourceBundle && this.getResourceBundle();
            if (this._oSearchSortDialog && this._oSearchSortDialog.isOpen && this._oSearchSortDialog.isOpen()) {
                this._oSearchSortDialog.close();
            }
            if (!this._oSearchGroupDialog) {
                this._oSearchGroupDialog = new ViewSettingsDialog({
                    title: oBundle && oBundle.getText("searchGroupDialogTitle") || "Group",
                    confirm: this.onSearchGroupDialogConfirm.bind(this)
                });
                resolveGroupItems(this).forEach(function (oItem) {
                    this._oSearchGroupDialog.addGroupItem(new ViewSettingsItem({ key: oItem.key, text: oItem.text }));
                }.bind(this));
                this.getView().addDependent(this._oSearchGroupDialog);
            }
            this._oSearchGroupDialog.setSelectedGroupItem(sSelectedKey || TOKENS.GROUP_NONE);
            this._oSearchGroupDialog.setGroupDescending(bSelectedDescending);
            this._oSearchGroupDialog.open("group");
        },

        onSearchGroupDialogConfirm: function (oEvent) {
            var oGroupItem = oEvent && oEvent.getParameter && oEvent.getParameter("groupItem");
            var bGroupDescending = !!(oEvent && oEvent.getParameter && oEvent.getParameter("groupDescending"));
            applySearchGroupSettings(this, {
                groupKey: oGroupItem && oGroupItem.getKey && oGroupItem.getKey(),
                groupDescending: bGroupDescending
            });
        },

        onOpenWorkflowAnalytics: function (oEvent) {
            return SearchViewBehavior.openWorkflowAnalytics(this);
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
            var sSelectedRowDisplayId = SearchSelectionRuntime.extractSelectedRowDisplayId(oEvent, oInnerTable);
            SearchCommandPolicy.selectionChanged(this, {
                event: oEvent,
                selectedRowId: sSelectedRowId,
                selectedRowDisplayId: sSelectedRowDisplayId,
                selectedRowIds: aSelectedRowIds,
                source: SEARCH_SOURCES.TABLE_SELECTION
            });
        },

        onSearchTableItemPress: function (oEvent) {
            var sRootId = resolvePressedSearchRowId(oEvent);
            if (!sRootId) {
                return undefined;
            }
            SearchViewBehavior.captureSearchScrollPosition(this);
            return SearchCommandPolicy.selectRow(this, {
                intent: SEARCH_SOURCES.OPEN,
                rootId: sRootId,
                source: SEARCH_SOURCES.TABLE_ITEM_PRESS
            });
        },

        onChecksFailSegmentChange: function (oEvent) {
            SearchCommandPolicy.buildFilter(this, { intent: SEARCH_SOURCES.CHECKS_SEGMENT, key: oEvent.getParameter("key") });
        },

        onBarriersFailSegmentChange: function (oEvent) {
            SearchCommandPolicy.buildFilter(this, { intent: SEARCH_SOURCES.BARRIERS_SEGMENT, key: oEvent.getParameter("key") });
        },

        _legacySmartTableContractHint: function () {
            return "SearchFilterBuilder.buildFailSegmentFilter";
        },

        onExportScreen: function () {
            return SearchViewBehavior.runExport(this, "screen");
        },

        onExportMenuAction: function (oEvent) {
            var oItem = oEvent.getParameter("item");
            return SearchViewBehavior.runExport(this, oItem && oItem.data("entity") || "screen");
        }
    };
});

