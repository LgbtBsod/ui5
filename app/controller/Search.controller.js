sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/Base.controller",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchActionBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchLifecycleBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchSmartTableBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchToolbarContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiSemanticConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime",
    "sap/ui/core/Item",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/RuntimeOrchestrationContracts",
    "sap/m/ViewSettingsDialog",
    "sap/m/ViewSettingsItem,\n    \"PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerStateRuntime\"",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsFormatRuntime"
], function (
    BaseController,
    SearchActionBehavior,
    SearchLifecycleBehavior,
    SearchSmartTableBehavior,
    ControllerViewStateRuntime,
    ModelStateRuntime,
    SearchToolbarContracts,
    SearchViewportRuntime,
    ModelContracts,
    OperationSourceContracts,
    SearchContracts,
    UiSemanticConstants,
    SearchViewStateRuntime,
    Item,
    JsRuntime,
    SchedulingRuntime,
    MessageKeyConstants,
    ReadinessTelemetryRuntime,
    ReadinessTelemetryContracts,
    ViewSettingsDialog,
    ViewSettingsItem,
    AnalyticsFormatRuntime
) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;
    var STATE_MODEL = MODELS.STATE;
    var PATHS = SearchToolbarContracts.PATHS;
    var SEARCH_MODE = SearchContracts.SEARCH_MODE;
    var TOKENS = ModelContracts.TOKENS;
    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var SEARCH_MESSAGE_KEYS = MessageKeyConstants.SEARCH;

    function resolveBundleText(oController, sKey) {
        var oBundle = oController && oController.getResourceBundle && oController.getResourceBundle();
        if (!sKey || !oBundle || !oBundle.getText) {
            return "";
        }
        return String(oBundle.getText(sKey) || "");
    }

    function formatSearchModeChipText(oController, sMode) {
        var sNorm = String(sMode || "").toUpperCase() === SEARCH_MODE.LOOSE ? SEARCH_MODE.LOOSE : SEARCH_MODE.EXACT;
        var sLabel = resolveBundleText(oController, SearchContracts.SEARCH_MODE_LABEL);
        var sModeText = sNorm === SEARCH_MODE.LOOSE
            ? resolveBundleText(oController, SearchContracts.SEARCH_MODE_LOOSE)
            : resolveBundleText(oController, SearchContracts.SEARCH_MODE_EXACT);
        return sLabel + ": " + sModeText;
    }

    function formatWorkflowStageText(oController, sStage) {
        return SearchViewStateRuntime.formatWorkflowStageText(
            oController && oController.getResourceBundle && oController.getResourceBundle(),
            sStage
        );
    }

    function formatWorkflowStageState(sStage) {
        return SearchViewStateRuntime.formatWorkflowStageState(sStage);
    }

    function formatSearchResultsCompactText(oController, iResultCount, bHasRows) {
        var iSafeCount = Math.max(0, Number(iResultCount || 0));
        var sResultsLabel = resolveBundleText(oController, SearchContracts.RESULTS_LABEL);
        if (!bHasRows || !iSafeCount) {
            return sResultsLabel;
        }
        return sResultsLabel + ": " + iSafeCount;
    }

    function formatSearchSelectionSummary(oController, iSelectionCount, sSelectedRowDisplayId) {
        var iSafeCount = Math.max(0, Number(iSelectionCount || 0));
        var sPrimaryId = String(sSelectedRowDisplayId || "").trim();
        if (!iSafeCount) {
            return resolveBundleText(oController, SearchContracts.SEARCH_SELECTION_NONE);
        }
        if (iSafeCount === 1 && sPrimaryId) {
            return resolveBundleText(oController, SearchContracts.SEARCH_SELECTION_PRIMARY_PREFIX) + ": " + sPrimaryId;
        }
        return iSafeCount + " " + resolveBundleText(oController, SearchContracts.SEARCH_SELECTION_UNITS);
    }

    function applyAnalyticsDrilldownIntent(oController) {
        return SearchLifecycleBehavior.applyAnalyticsDrilldownIntent(oController, {
            intentPath: PATHS.ANALYTICS_DRILLDOWN_INTENT,
            smartTableReadyPath: "/smartTableReady",
            source: SEARCH_SOURCES.ANALYTICS_DRILLDOWN,
            stateModel: STATE_MODEL
        });
    }

    function bindLocationSuggest(oController) {
        var oSmartFilterBar = oController.byId("smartFilterBar");
        var oLocationControl;
        if (!oSmartFilterBar || typeof oSmartFilterBar.getControlByKey !== TYPE_FUNCTION) {
            return;
        }
        oLocationControl = oSmartFilterBar.getControlByKey("LocationKey");
        if (!oLocationControl || oLocationControl.data("locationSuggestBound")) {
            return;
        }
        if (typeof oLocationControl.setShowSuggestion === TYPE_FUNCTION) {
            oLocationControl.setShowSuggestion(true);
        }
        if (typeof oLocationControl.attachSuggest === TYPE_FUNCTION) {
            oLocationControl.attachSuggest(oController.onLocationKeySuggest, oController);
        }
        if (typeof oLocationControl.attachSuggestionItemSelected === TYPE_FUNCTION) {
            oLocationControl.attachSuggestionItemSelected(oController.onLocationKeySuggestionSelected, oController);
        }
        oLocationControl.data("locationSuggestBound", true);
    }

    function updateLocationSuggestions(oControl, aItems) {
        if (!oControl || typeof oControl.destroySuggestionItems !== TYPE_FUNCTION || typeof oControl.addSuggestionItem !== TYPE_FUNCTION) {
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
    }

    function runLocationSuggest(oController, oEvent) {
        var sValue = String(oEvent && oEvent.getParameter && (oEvent.getParameter("suggestValue") || oEvent.getParameter("value")) || "").trim();
        var oControl = oEvent && oEvent.getSource && oEvent.getSource();
        var oCtx = oController._ctx && oController._ctx();
        var oLookup = oCtx && oCtx.locationLookup;
        var sNeedle = sValue.toLowerCase();
        oController._iLocationSuggestTimer = SchedulingRuntime.clearTimer(oController._iLocationSuggestTimer);
        oController._iLocationSuggestTimer = SchedulingRuntime.restartTimer(0, function () {
            var iRequestVersion;
            oController._iLocationSuggestTimer = null;
            if (!oControl) {
                return;
            }
            iRequestVersion = Number(oController._iLocationSuggestRequestVersion || 0) + 1;
            oController._iLocationSuggestRequestVersion = iRequestVersion;
            if (sNeedle && Array.isArray(oController._aLocationSuggestCache) && oController._aLocationSuggestCache.length && oController._sLocationSuggestNeedle && sNeedle.indexOf(oController._sLocationSuggestNeedle) === 0) {
                updateLocationSuggestions(oControl, oController._aLocationSuggestCache.filter(function (oItem) {
                    var sCode = String((oItem && (oItem.location_code || oItem.location_id)) || "").toLowerCase();
                    var sName = String((oItem && oItem.location_name) || "").toLowerCase();
                    return sCode.indexOf(sNeedle) >= 0 || sName.indexOf(sNeedle) >= 0;
                }));
                return;
            }
            if (!oLookup || typeof oLookup.search !== TYPE_FUNCTION) {
                updateLocationSuggestions(oControl, []);
                return;
            }
            Promise.resolve(oLookup.search({ query: sValue, limit: 50 }))
                .then(function (oFound) {
                    var aItems;
                    if (oController._iLocationSuggestRequestVersion !== iRequestVersion) {
                        return;
                    }
                    aItems = (oFound && oFound.items) || [];
                    oController._aLocationSuggestCache = aItems;
                    oController._sLocationSuggestNeedle = sNeedle;
                    updateLocationSuggestions(oControl, aItems);
                })
                .catch(function () {
                    if (oController._iLocationSuggestRequestVersion !== iRequestVersion) {
                        return;
                    }
                    updateLocationSuggestions(oControl, []);
                });
        }, 180);
    }

    function applyLocationSuggestionSelection(oEvent) {
        var oSelected = oEvent && oEvent.getParameter && oEvent.getParameter("selectedItem");
        var oControl = oEvent && oEvent.getSource && oEvent.getSource();
        if (!oSelected || !oControl || typeof oControl.setValue !== TYPE_FUNCTION) {
            return;
        }
        oControl.setValue(oSelected.getKey ? oSelected.getKey() : oSelected.getText());
    }

    function resolveDialogItems(oController, aItems) {
        return (aItems || []).map(function (oItem) {
            return {
                key: oItem.key,
                text: resolveBundleText(oController, oItem.textKey) || oItem.fallback
            };
        });
    }

    function ensureSettingsDialog(oController, sDialogKey, mConfig) {
        var sInstanceKey = sDialogKey === "sort" ? "_oSearchSortDialog" : "_oSearchGroupDialog";
        if (!oController[sInstanceKey]) {
            oController[sInstanceKey] = new ViewSettingsDialog({
                title: resolveBundleText(oController, mConfig.titleKey) || mConfig.titleFallback,
                confirm: mConfig.onConfirm.bind(oController)
            });
            resolveDialogItems(oController, mConfig.items).forEach(function (oItem) {
                oController[sInstanceKey][mConfig.addItemMethod](new ViewSettingsItem({ key: oItem.key, text: oItem.text }));
            });
            oController.getView().addDependent(oController[sInstanceKey]);
        }
        return oController[sInstanceKey];
    }

    function shouldRebindSearch(oController) {
        return !!(ControllerViewStateRuntime.get(oController, "/hasSearched")
            && ControllerViewStateRuntime.get(oController, "/smartTableReady"));
    }

    function buildSortSettingsFromEvent(oEvent) {
        var oSortItem = oEvent && oEvent.getParameter && oEvent.getParameter("sortItem");
        return {
            sortKey: oSortItem && oSortItem.getKey && oSortItem.getKey(),
            sortDescending: !!(oEvent && oEvent.getParameter && oEvent.getParameter("sortDescending"))
        };
    }

    function buildGroupSettingsFromEvent(oEvent) {
        var oGroupItem = oEvent && oEvent.getParameter && oEvent.getParameter("groupItem");
        return {
            groupKey: oGroupItem && oGroupItem.getKey && oGroupItem.getKey(),
            groupDescending: !!(oEvent && oEvent.getParameter && oEvent.getParameter("groupDescending"))
        };
    }

    function applySearchSortSettings(oController, mSettings) {
        var sSortKey = String((mSettings && mSettings.sortKey) || "").trim() || TOKENS.DATE_CHECK;
        var bSortDescending = !!(mSettings && mSettings.sortDescending);
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_SORT_KEY, sSortKey);
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_SORT_DESCENDING, bSortDescending);
        if (shouldRebindSearch(oController)) {
            return SearchActionBehavior.runSearchCommand(oController, SearchContracts.COMMANDS.REBIND, {
                source: OperationSourceContracts.SEARCH.SEARCH_SORT_SETTINGS
            });
        }
        return Promise.resolve({ source: OperationSourceContracts.SEARCH.SEARCH_SORT_SETTINGS, skipped: true });
    }

    function applySearchGroupSettings(oController, mSettings) {
        var sGroupKey = String((mSettings && mSettings.groupKey) || "").trim();
        var bGroupDescending = !!(mSettings && mSettings.groupDescending);
        if (sGroupKey === TOKENS.GROUP_NONE) {
            sGroupKey = "";
        }
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_GROUP_KEY, sGroupKey);
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_GROUP_DESCENDING, bGroupDescending);
        if (shouldRebindSearch(oController)) {
            return SearchActionBehavior.runSearchCommand(oController, SearchContracts.COMMANDS.REBIND, {
                source: OperationSourceContracts.SEARCH.SEARCH_GROUP_SETTINGS
            });
        }
        return Promise.resolve({ source: OperationSourceContracts.SEARCH.SEARCH_GROUP_SETTINGS, skipped: true });
    }

    function openSortDialog(oController) {
        var oGroupDialog = oController._oSearchGroupDialog;
        var oDialog;
        if (oGroupDialog && typeof oGroupDialog.isOpen === TYPE_FUNCTION && oGroupDialog.isOpen()) {
            oGroupDialog.close();
        }
        oDialog = ensureSettingsDialog(oController, "sort", {
            titleKey: SEARCH_MESSAGE_KEYS.SORT_DIALOG_TITLE,
            titleFallback: "",
            items: SearchToolbarContracts.SORT_ITEMS,
            addItemMethod: "addSortItem",
            onConfirm: oController.onSearchSortDialogConfirm
        });
        oDialog.setSelectedSortItem(String(ModelStateRuntime.read(oController, STATE_MODEL, PATHS.SEARCH_SORT_KEY, SearchToolbarContracts.DEFAULTS.SORT_KEY) || SearchToolbarContracts.DEFAULTS.SORT_KEY));
        oDialog.setSortDescending(!!ModelStateRuntime.read(oController, STATE_MODEL, PATHS.SEARCH_SORT_DESCENDING, true));
        oDialog.open("sort");
        ReadinessTelemetryRuntime.markControllerStage(oController, ReadinessTelemetryContracts.STAGES.DEFERRED_DIALOG_READY, {
            dialog: "searchSort"
        });
    }

    function openGroupDialog(oController) {
        var oSortDialog = oController._oSearchSortDialog;
        var oDialog;
        if (oSortDialog && typeof oSortDialog.isOpen === TYPE_FUNCTION && oSortDialog.isOpen()) {
            oSortDialog.close();
        }
        oDialog = ensureSettingsDialog(oController, "group", {
            titleKey: SEARCH_MESSAGE_KEYS.GROUP_DIALOG_TITLE,
            titleFallback: "",
            items: SearchToolbarContracts.GROUP_ITEMS,
            addItemMethod: "addGroupItem",
            onConfirm: oController.onSearchGroupDialogConfirm
        });
        oDialog.setSelectedGroupItem(String(ModelStateRuntime.read(oController, STATE_MODEL, PATHS.SEARCH_GROUP_KEY, "") || SearchToolbarContracts.DEFAULTS.GROUP_KEY));
        oDialog.setGroupDescending(!!ModelStateRuntime.read(oController, STATE_MODEL, PATHS.SEARCH_GROUP_DESCENDING, false));
        oDialog.open("group");
        ReadinessTelemetryRuntime.markControllerStage(oController, ReadinessTelemetryContracts.STAGES.DEFERRED_DIALOG_READY, {
            dialog: "searchGroup"
        });
    }

    return BaseController.extend("PRODUCTION_CONTROL_CHECKLIST.controller.Search", {
        _withActionBusy: function (sPath, fnAction) {
            return SearchActionBehavior.withActionBusy(this, sPath, fnAction);
        },
        onInit: function () { SearchLifecycleBehavior.onInit(this); },
        onAfterRendering: function () { SearchLifecycleBehavior.onAfterRendering(this); },
        onExit: function () { SearchLifecycleBehavior.onExit(this); },
        _onSearchMatched: function () {
            SearchLifecycleBehavior.onSearchMatched(this, applyAnalyticsDrilldownIntent.bind(null, this));
        },
        _onDetailSearchContextMatched: function (oEvent) {
            SearchLifecycleBehavior.onDetailSearchContextMatched(this, oEvent);
        },
        _onAnalyticsMatched: function () {
            SearchLifecycleBehavior.onAnalyticsMatched(this);
        },
        onSmartFilterInitialise: function () {
            SearchSmartTableBehavior.onSmartFilterInitialise(this, applyAnalyticsDrilldownIntent.bind(null, this));
        },
        onLocationKeySuggest: function (oEvent) {
            runLocationSuggest(this, oEvent);
        },
        onLocationKeySuggestionSelected: function (oEvent) {
            applyLocationSuggestionSelection(oEvent);
        },
        onSmartFilterChanged: function () { SearchSmartTableBehavior.onSmartFilterChanged(this); },
        onSmartFilterClear: function () { SearchSmartTableBehavior.onSmartFilterClear(this); },
        onSmartTableInitialise: function () {
            SearchSmartTableBehavior.onSmartTableInitialise(this, this._readSearchRows.bind(this));
        },
        onBeforeSmartTableRebind: function (oEvent) {
            SearchSmartTableBehavior.onBeforeSmartTableRebind(this, oEvent, this._readSearchRows.bind(this));
        },
        onSmartSearch: function () { return SearchActionBehavior.onSmartSearch(this); },
        onRetrySearchLoad: function () {
            if (!this._facade || !this._facade.rebind) {
                return Promise.resolve();
            }
            return SearchActionBehavior.onRetrySearchLoad(this);
        },
        onCreate: function () { return SearchActionBehavior.onCreate(this); },
        onCopy: function () { return SearchActionBehavior.onCopy(this); },
        onSelectVisibleRows: function () { return SearchActionBehavior.onSelectVisibleRows(this); },
        onClearSelection: function () { return SearchActionBehavior.onClearSelection(this); },
        onScrollSearchAnchor: function () { return SearchViewportRuntime.scrollToSearchFilters(this); },
        onScrollSearchResultsToolbarAnchor: function () { return SearchViewportRuntime.scrollToSearchResultsToolbar(this); },
        onMaxRowsChange: function (oEvent) { SearchSmartTableBehavior.onMaxRowsChange(this, oEvent); },
        onBackendTopChange: function (oEvent) { SearchSmartTableBehavior.onBackendTopChange(this, oEvent); },
        onSearchModeToggle: function (oEvent) { SearchSmartTableBehavior.onSearchModeToggle(this, oEvent); },
        formatSearchModeChipText: function (sMode) { return formatSearchModeChipText(this, sMode); },
        formatSearchResultsCompactText: function (iResultCount, bHasRows) {
            return formatSearchResultsCompactText(this, iResultCount, bHasRows);
        },
        formatSearchSelectionSummary: function (iSelectionCount, sSelectedRowDisplayId) {
            return formatSearchSelectionSummary(this, iSelectionCount, sSelectedRowDisplayId);
        },
        formatAnalyticsSourceText: function (sSelectedSource) {
            return AnalyticsFormatRuntime.formatSourceText(sSelectedSource, resolveBundleText.bind(null, this));
        },
        formatAnalyticsSourceContext: function (sSelectedSource) {
            return AnalyticsFormatRuntime.formatSourceContext(sSelectedSource, resolveBundleText.bind(null, this));
        },
        formatSelectionSummaryState: function () { return UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION; },
        formatLoadErrorType: function () { return UiSemanticConstants.MESSAGE_TYPE.ERROR; },
        formatSearchModeState: function () { return UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION; },
        formatToolbarSelectionState: function (iSelectionCount) {
            return Number(iSelectionCount || 0) > 0
                ? UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS
                : UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION;
        },
        onOpenSearchSortDialog: function () { openSortDialog(this); },
        onSearchSortDialogConfirm: function (oEvent) {
            return applySearchSortSettings(this, buildSortSettingsFromEvent(oEvent));
        },
        onOpenSearchGroupDialog: function () { openGroupDialog(this); },
        onSearchGroupDialogConfirm: function (oEvent) {
            return applySearchGroupSettings(this, buildGroupSettingsFromEvent(oEvent));
        },
        onOpenWorkflowAnalytics: function (oEvent) {
            return SearchActionBehavior.onOpenWorkflowAnalytics(this, oEvent);
        },
        formatWorkflowStageText: function (sStage) { return formatWorkflowStageText(this, sStage); },
        formatWorkflowStageState: function (sStage) { return formatWorkflowStageState(sStage); },
        onSearchTableSelectionChange: function (oEvent) {
            SearchActionBehavior.onTableSelectionChange(this, oEvent);
        },
        onSearchTableItemPress: function (oEvent) {
            return SearchActionBehavior.onTableItemPress(this, oEvent);
        },
        onChecksFailSegmentChange: function (oEvent) {
            SearchActionBehavior.runSearchCommand(this, SearchContracts.COMMANDS.BUILD_FILTER, {
                intent: SEARCH_SOURCES.CHECKS_SEGMENT,
                key: oEvent.getParameter("key")
            });
        },
        onBarriersFailSegmentChange: function (oEvent) {
            SearchActionBehavior.runSearchCommand(this, SearchContracts.COMMANDS.BUILD_FILTER, {
                intent: SEARCH_SOURCES.BARRIERS_SEGMENT,
                key: oEvent.getParameter("key")
            });
        },
        onExportScreen: function () { return SearchActionBehavior.onExportScreen(this); },
        onExportMenuAction: function (oEvent) { return SearchActionBehavior.onExportMenuAction(this, oEvent); },
        _readSearchRows: function (oInnerTable) {
            var aRows = [];
            var oCtx = this._ctx && this._ctx();
            if (oCtx && oCtx.smartControls && oCtx.smartControls.getVisibleRows) {
                aRows = oCtx.smartControls.getVisibleRows() || [];
            }
            if (!aRows.length && oInnerTable) {
                aRows = oInnerTable.getItems ? (oInnerTable.getItems() || []) : [];
            }
            return aRows;
        },
        _bindLocationSuggest: function () {
            bindLocationSuggest(this);
        }
    });
});
