sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiControlIds"
], function (SearchSelectionStateRuntime, ControllerViewStateRuntime, ModelStateRuntime, FocusRuntime, SearchUiContracts, JsRuntime, ModelContracts, UiControlIds) {
    "use strict";

    var SEARCH_COLUMN_RULES = SearchUiContracts.COLUMN_RULES;
    var COMPACT_VIEWPORT_REM_MAX = SearchUiContracts.VIEWPORT.COMPACT_REM_MAX;
    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var TYPE_OBJECT = JsRuntime.TYPEOF.OBJECT;
    var METHODS = JsRuntime.METHODS;
    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var FILTER_CONTROL_SELECTORS = ["input", "button", "[role='search']", "[role='combobox']"];
    var RESULTS_CONTROL_SELECTORS = ["[role='row']", "[role='grid']", "[role='table']", "tr", ".sapMLIB", ".sapMListTblRow"];
    var TOOLBAR_CONTROL_SELECTORS = ["input", "button", "[role='button']", "[role='combobox']"];
    var FILTER_VIEW_SELECTORS = [
        ".searchFilterSurface input",
        ".searchFilterSurface button",
        "[role='search'] input",
        "[role='search'] button"
    ];
    var RESULTS_VIEW_SELECTORS = [
        ".searchResultsTable [role='row']",
        ".searchResultsTable .sapMLIB",
        ".searchResultsTable .sapMListTblRow",
        ".searchResultsTable [role='grid']",
        ".searchResultsTable [role='table']"
    ];
    var TOOLBAR_VIEW_SELECTORS = [
        ".searchResultsActionRail input",
        ".searchResultsActionRail button",
        ".searchResultsActionRail [role='button']",
        ".searchResultsActionRail [role='combobox']",
        ".searchSettingsRail input",
        ".searchSettingsRail button",
        ".searchSettingsRail [role='button']",
        ".searchSettingsRail [role='combobox']"
    ];

    function resolveSearchInnerTable(oController) {
        var oSmartTable = oController.byId(UiControlIds.SEARCH.SMART_TABLE);
        return oSmartTable && oSmartTable.getTable && oSmartTable.getTable();
    }

    function resolveSmartSearchButton(oController) {
        return oController && oController.byId
            ? oController.byId(UiControlIds.SEARCH.SMART_FILTER_BAR + UiControlIds.SEARCH.GO_BUTTON_SUFFIX)
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

    function parseColumnPersonalizationData(oColumn) {
        var vData = oColumn && oColumn.data && oColumn.data("p13nData");
        if (!vData) {
            return null;
        }
        if (typeof vData === "string") {
            try {
                return JSON.parse(vData);
            } catch (_error) {
                return null;
            }
        }
        return typeof vData === TYPE_OBJECT ? vData : null;
    }

    function resolveSearchColumnKey(oColumn) {
        var oP13nData = parseColumnPersonalizationData(oColumn) || {};
        var sKey = oP13nData.columnKey || oP13nData.leadingProperty || oP13nData.sortProperty || oP13nData.filterProperty || "";
        var oHeader = oColumn && oColumn.getHeader && oColumn.getHeader();
        var sHeaderText = oHeader && oHeader.getText && oHeader.getText();
        return String(sKey || sHeaderText || "");
    }

    function resolveSearchViewportWidth(oController) {
        var oResultsShell = oController && oController.byId && oController.byId(UiControlIds.SEARCH.RESULTS_SHELL);
        var oResultsShellDom = oResultsShell && oResultsShell.getDomRef && oResultsShell.getDomRef();
        var oViewDom = oController && oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
        var iWidth = 0;
        if (oResultsShellDom && oResultsShellDom.getBoundingClientRect) {
            iWidth = Math.floor(oResultsShellDom.getBoundingClientRect().width || 0);
        }
        if (!iWidth && oViewDom && oViewDom.getBoundingClientRect) {
            iWidth = Math.floor(oViewDom.getBoundingClientRect().width || 0);
        }
        if (!iWidth && typeof window !== "undefined") {
            iWidth = Math.floor(window.innerWidth || 0);
        }
        return iWidth || 0;
    }

    function isCompactSearchViewport(oController) {
        var iRootSize = parseFloat(window.getComputedStyle(document.documentElement).fontSize || "16");
        var iViewportRem = resolveSearchViewportWidth(oController) / (Number.isFinite(iRootSize) && iRootSize > 0 ? iRootSize : 16);
        return iViewportRem <= COMPACT_VIEWPORT_REM_MAX;
    }

    function applySearchColumnRule(oController, oColumn, mRule, sColumnKey) {
        var bCompactViewport = isCompactSearchViewport(oController);
        var bBaseVisible;
        if (!oColumn || !mRule) {
            return;
        }
        if (typeof oColumn.data === TYPE_FUNCTION && typeof oColumn.data("chkBaseVisible") !== JsRuntime.TYPEOF.BOOLEAN) {
            oColumn.data("chkBaseVisible", !(typeof oColumn.getVisible === "function") || oColumn.getVisible());
        }
        bBaseVisible = typeof oColumn.data === TYPE_FUNCTION && typeof oColumn.data("chkBaseVisible") === JsRuntime.TYPEOF.BOOLEAN
            ? oColumn.data("chkBaseVisible")
            : true;
        if (typeof oColumn.setWidth === TYPE_FUNCTION) {
            oColumn.setWidth(bCompactViewport ? "auto" : (mRule.width || "auto"));
        }
        if (typeof oColumn.setMinScreenWidth === TYPE_FUNCTION) {
            oColumn.setMinScreenWidth(mRule.minScreenWidth || "");
        }
        if (typeof oColumn.setDemandPopin === TYPE_FUNCTION) {
            oColumn.setDemandPopin(!!mRule.demandPopin);
        }
        if (typeof oColumn.setImportance === TYPE_FUNCTION && mRule.importance) {
            oColumn.setImportance(mRule.importance);
        }
        if (typeof oColumn.setPopinDisplay === TYPE_FUNCTION) {
            oColumn.setPopinDisplay(bCompactViewport ? "Block" : "Inline");
        }
        if (typeof oColumn.setVisible === TYPE_FUNCTION) {
            oColumn.setVisible(!!bBaseVisible);
        }
        if (typeof oColumn.setHAlign === TYPE_FUNCTION && (sColumnKey === "SuccessChecksRate" || sColumnKey === "SuccessBarriersRate")) {
            oColumn.setHAlign("Center");
        }
        if (typeof oColumn.toggleStyleClass === TYPE_FUNCTION) {
            oColumn.toggleStyleClass("searchColumnCritical", mRule.importance === "High");
            oColumn.toggleStyleClass("searchColumnSecondary", mRule.importance === "Low");
            oColumn.toggleStyleClass("searchColumnHiddenNarrow", false);
        }
    }

    function configureSearchResultTable(oController, oInnerTable, bForce) {
        var aColumns;
        var bCompactViewport = isCompactSearchViewport(oController);
        var iViewportWidth = resolveSearchViewportWidth(oController);
        var sTableId;
        var sLayoutKey;
        if (!oInnerTable) {
            return;
        }
        sTableId = oInnerTable && oInnerTable.getId ? oInnerTable.getId() : "searchInnerTable";
        sLayoutKey = [sTableId, bCompactViewport ? "compact" : "regular", iViewportWidth].join("::");
        if (!bForce && oController._sSearchTableLayoutKey === sLayoutKey) {
            return;
        }
        if (typeof oInnerTable.setFixedLayout === TYPE_FUNCTION) {
            oInnerTable.setFixedLayout(bCompactViewport);
        }
        if (typeof oInnerTable.setAutoPopinMode === TYPE_FUNCTION) {
            oInnerTable.setAutoPopinMode(false);
        }
        aColumns = oInnerTable.getColumns ? (oInnerTable.getColumns() || []) : [];
        aColumns.forEach(function (oColumn) {
            var sColumnKey = resolveSearchColumnKey(oColumn);
            applySearchColumnRule(oController, oColumn, SEARCH_COLUMN_RULES[sColumnKey], sColumnKey);
        });
        oController._sSearchTableLayoutKey = sLayoutKey;
    }

    function resolveSearchSelectionMode(oController) {
        var sSelectionMode = String(
            ModelStateRuntime.read(oController, STATE_MODEL, "/smartTable/selectionMode", "MultiSelect")
        ).trim() || "MultiSelect";
        return sSelectionMode === "SingleSelectMaster" ? "MultiSelect" : sSelectionMode;
    }

    function resolveSearchTableCounts(oInnerTable) {
        var aItems = oInnerTable && oInnerTable.getItems ? (oInnerTable.getItems() || []) : [];
        var oBinding = oInnerTable && oInnerTable.getBinding ? oInnerTable.getBinding("items") : null;
        var iVisibleCount = aItems.length;
        var iTotalCount = oBinding && oBinding.getLength ? Number(oBinding.getLength()) : iVisibleCount;
        if (!Number.isFinite(iTotalCount) || iTotalCount < 0) {
            iTotalCount = iVisibleCount;
        }
        return {
            visibleCount: iVisibleCount,
            totalCount: iTotalCount,
            hasRows: iTotalCount > 0 || iVisibleCount > 0
        };
    }

    function syncSearchTableRuntimeState(oController, oInnerTable) {
        var sSelectionMode;
        var mCounts;
        if (!oInnerTable) {
            return null;
        }
        sSelectionMode = resolveSearchSelectionMode(oController);
        if (typeof oInnerTable.setMode === TYPE_FUNCTION) {
            oInnerTable.setMode(sSelectionMode);
        }
        if (typeof oInnerTable.setIncludeItemInSelection === TYPE_FUNCTION) {
            oInnerTable.setIncludeItemInSelection(false);
        }
        mCounts = resolveSearchTableCounts(oInnerTable);
        ControllerViewStateRuntime.setMany(oController, {
            "/hasRows": mCounts.hasRows,
            "/canExport": mCounts.hasRows
        });
        return mCounts;
    }

    function bindSearchTableRuntime(oController, oInnerTable, fnOnRuntimeChanged) {
        if (!oInnerTable || oInnerTable.data("searchRuntimeBound")) {
            syncSearchTableRuntimeState(oController, oInnerTable);
            return;
        }
        if (typeof oInnerTable.attachUpdateFinished === TYPE_FUNCTION) {
            oInnerTable.attachUpdateFinished(function () {
                syncSearchTableRuntimeState(oController, oInnerTable);
                if (typeof fnOnRuntimeChanged === TYPE_FUNCTION) {
                    fnOnRuntimeChanged();
                }
            });
        }
        oInnerTable.data("searchRuntimeBound", true);
        syncSearchTableRuntimeState(oController, oInnerTable);
    }

    function focusDomNode(oNode) {
        if (!oNode || typeof oNode[METHODS.FOCUS] !== TYPE_FUNCTION) {
            return false;
        }
        try {
            oNode[METHODS.FOCUS]();
            return true;
        } catch (_focusError) {
            return false;
        }
    }

    function resolveDomRef(oControl) {
        if (!oControl) {
            return null;
        }
        if (typeof oControl[METHODS.GET_FOCUS_DOM_REF] === TYPE_FUNCTION) {
            return oControl[METHODS.GET_FOCUS_DOM_REF]();
        }
        if (typeof oControl[METHODS.GET_DOM_REF] === TYPE_FUNCTION) {
            return oControl[METHODS.GET_DOM_REF]();
        }
        return null;
    }

    function resolveViewDomRef(oController) {
        var oView = oController && typeof oController.getView === TYPE_FUNCTION ? oController.getView() : null;
        return oView && typeof oView[METHODS.GET_DOM_REF] === TYPE_FUNCTION ? oView[METHODS.GET_DOM_REF]() : null;
    }

    function queryFirstFocusable(oRoot, aSelectors) {
        var i;
        var oCandidate;
        if (!oRoot || typeof oRoot.querySelector !== TYPE_FUNCTION || !Array.isArray(aSelectors)) {
            return null;
        }
        for (i = 0; i < aSelectors.length; i += 1) {
            oCandidate = aSelectors[i] ? oRoot.querySelector(aSelectors[i]) : null;
            if (oCandidate) {
                return oCandidate;
            }
        }
        return null;
    }

    function focusControl(oControl, aSelectors) {
        var oDomRef;
        var oFocusable;
        if (!oControl) {
            return false;
        }
        if (typeof oControl[METHODS.FOCUS] === TYPE_FUNCTION) {
            try {
                oControl[METHODS.FOCUS]();
                return true;
            } catch (_focusError) {
                // Fall back to DOM focus below.
            }
        }
        if (FocusRuntime.focusSoon(oControl)) {
            return true;
        }
        oDomRef = resolveDomRef(oControl);
        oFocusable = queryFirstFocusable(oDomRef, aSelectors || []);
        return focusDomNode(oFocusable || oDomRef);
    }

    function focusViewScopedFallback(oController, aSelectors) {
        var oViewDomRef = resolveViewDomRef(oController);
        var oFocusable = queryFirstFocusable(oViewDomRef, aSelectors || []);
        return focusDomNode(oFocusable);
    }

    function resolveToolbarControl(oController) {
        return oController.byId("backendTopInput")
            || oController.byId("maxRowsInput")
            || oController.byId("searchActionRailStack")
            || oController.byId("searchResultsActionRail");
    }

    function resolveFilterControl(oController) {
        return oController.byId(UiControlIds.SEARCH.SMART_FILTER_BAR);
    }

    function resolveResultsTarget(oController) {
        var oInnerTable = resolveSearchInnerTable(oController);
        var aSelectedItems;
        var aItems;
        if (!oInnerTable) {
            return oController.byId(UiControlIds.SEARCH.SMART_TABLE);
        }
        aSelectedItems = typeof oInnerTable.getSelectedItems === TYPE_FUNCTION ? (oInnerTable.getSelectedItems() || []) : [];
        if (Array.isArray(aSelectedItems) && aSelectedItems.length) {
            return aSelectedItems[0];
        }
        aItems = typeof oInnerTable.getItems === TYPE_FUNCTION ? (oInnerTable.getItems() || []) : [];
        if (Array.isArray(aItems) && aItems.length) {
            return aItems[0];
        }
        return oInnerTable;
    }

    return {
        bindSearchTableRuntime: bindSearchTableRuntime,
        clearSelection: clearSelection,
        configureSearchResultTable: configureSearchResultTable,
        extractSelectedRowDisplayId: SearchSelectionStateRuntime.extractSelectedRowDisplayId,
        extractSelectedRowId: SearchSelectionStateRuntime.extractSelectedRowId,
        extractSelectedRowIds: SearchSelectionStateRuntime.extractSelectedRowIds,
        focusSearchFilters: function (oController) {
            return focusControl(resolveFilterControl(oController), FILTER_CONTROL_SELECTORS)
                || focusViewScopedFallback(oController, FILTER_VIEW_SELECTORS);
        },
        focusSearchResults: function (oController) {
            return focusControl(resolveResultsTarget(oController), RESULTS_CONTROL_SELECTORS)
                || focusViewScopedFallback(oController, RESULTS_VIEW_SELECTORS);
        },
        focusSearchToolbar: function (oController) {
            return focusControl(resolveToolbarControl(oController), TOOLBAR_CONTROL_SELECTORS)
                || focusViewScopedFallback(oController, TOOLBAR_VIEW_SELECTORS);
        },
        resolveSmartSearchButton: resolveSmartSearchButton,
        resolveSearchInnerTable: resolveSearchInnerTable,
        selectVisibleRows: selectVisibleRows,
        syncSearchTableRuntimeState: syncSearchTableRuntimeState
    };
});
