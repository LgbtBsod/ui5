sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ChecklistIdentity",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/SearchUiContracts"
], function (FocusRuntime, ControllerViewStateRuntime, ModelStateRuntime, SchedulingRuntime, ChecklistIdentity, SearchUiContracts) {
    "use strict";

    var SEARCH_COLUMN_RULES = SearchUiContracts.COLUMN_RULES;
    var COMPACT_VIEWPORT_REM_MAX = SearchUiContracts.VIEWPORT.COMPACT_REM_MAX;

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
        return typeof vData === "object" ? vData : null;
    }

    function resolveSearchColumnKey(oColumn) {
        var oP13nData = parseColumnPersonalizationData(oColumn) || {};
        var sKey = oP13nData.columnKey || oP13nData.leadingProperty || oP13nData.sortProperty || oP13nData.filterProperty || "";
        var oHeader = oColumn && oColumn.getHeader && oColumn.getHeader();
        var sHeaderText = oHeader && oHeader.getText && oHeader.getText();
        return String(sKey || sHeaderText || "");
    }

    function resolveSearchViewportWidth(oController) {
        var oSearchHost = oController && oController.byId && oController.byId("searchPaneHost");
        var oSearchHostDom = oSearchHost && oSearchHost.getDomRef && oSearchHost.getDomRef();
        var oResultsShell = oController && oController.byId && oController.byId("searchResultsShell");
        var oResultsShellDom = oResultsShell && oResultsShell.getDomRef && oResultsShell.getDomRef();
        var iWidth = 0;
        if (oResultsShellDom && oResultsShellDom.getBoundingClientRect) {
            iWidth = Math.floor(oResultsShellDom.getBoundingClientRect().width || 0);
        }
        if (!iWidth && oSearchHostDom && oSearchHostDom.getBoundingClientRect) {
            iWidth = Math.floor(oSearchHostDom.getBoundingClientRect().width || 0);
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
        if (typeof oColumn.data === "function" && typeof oColumn.data("chkBaseVisible") !== "boolean") {
            oColumn.data("chkBaseVisible", !(typeof oColumn.getVisible === "function") || oColumn.getVisible());
        }
        bBaseVisible = typeof oColumn.data === "function" && typeof oColumn.data("chkBaseVisible") === "boolean"
            ? oColumn.data("chkBaseVisible")
            : true;
        if (typeof oColumn.setWidth === "function") {
            oColumn.setWidth(bCompactViewport ? "auto" : (mRule.width || "auto"));
        }
        if (typeof oColumn.setMinScreenWidth === "function") {
            oColumn.setMinScreenWidth(mRule.minScreenWidth || "");
        }
        if (typeof oColumn.setDemandPopin === "function") {
            oColumn.setDemandPopin(!!mRule.demandPopin);
        }
        if (typeof oColumn.setImportance === "function" && mRule.importance) {
            oColumn.setImportance(mRule.importance);
        }
        if (typeof oColumn.setPopinDisplay === "function") {
            oColumn.setPopinDisplay(bCompactViewport ? "Block" : "Inline");
        }
        if (typeof oColumn.setVisible === "function") {
            oColumn.setVisible(!!bBaseVisible);
        }
        if (typeof oColumn.setHAlign === "function" && (sColumnKey === "SuccessChecksRate" || sColumnKey === "SuccessBarriersRate")) {
            oColumn.setHAlign("Center");
        }
        if (typeof oColumn.toggleStyleClass === "function") {
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
        if (typeof oInnerTable.setFixedLayout === "function") {
            oInnerTable.setFixedLayout(bCompactViewport);
        }
        if (typeof oInnerTable.setAutoPopinMode === "function") {
            oInnerTable.setAutoPopinMode(false);
        }
        aColumns = oInnerTable.getColumns ? (oInnerTable.getColumns() || []) : [];
        aColumns.forEach(function (oColumn) {
            var sColumnKey = resolveSearchColumnKey(oColumn);
            applySearchColumnRule(oController, oColumn, SEARCH_COLUMN_RULES[sColumnKey], sColumnKey);
        });
        oController._sSearchTableLayoutKey = sLayoutKey;
    }

    function resolveSmartSearchButton(oController) {
        var oSmartFilterBar = oController.byId("searchSmartFilterBar");
        var aButtons;
        if (!oSmartFilterBar || typeof oSmartFilterBar.findAggregatedObjects !== "function") {
            return null;
        }
        aButtons = oSmartFilterBar.findAggregatedObjects(true, function (oCandidate) {
            var sName = oCandidate && oCandidate.getMetadata && oCandidate.getMetadata().getName();
            if (sName !== "sap.m.Button") {
                return false;
            }
            return typeof oCandidate.getType === "function" && oCandidate.getType() === "Emphasized";
        }) || [];
        return aButtons[0] || null;
    }

    function resolveSearchInnerTable(oController) {
        var oSmartTable = oController.byId("searchSmartTable");
        return oSmartTable && oSmartTable.getTable && oSmartTable.getTable();
    }

    function resolveSearchSelectionMode(oController) {
        var sSelectionMode = String(
            ModelStateRuntime.read(oController, "state", "/smartTable/selectionMode", "MultiSelect")
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
        if (oInnerTable.setMode) {
            oInnerTable.setMode(sSelectionMode);
        }
        if (oInnerTable.setIncludeItemInSelection) {
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
        if (oInnerTable.attachUpdateFinished) {
            oInnerTable.attachUpdateFinished(function () {
                syncSearchTableRuntimeState(oController, oInnerTable);
                if (typeof fnOnRuntimeChanged === "function") {
                    fnOnRuntimeChanged();
                }
            });
        }
        oInnerTable.data("searchRuntimeBound", true);
        syncSearchTableRuntimeState(oController, oInnerTable);
    }

    function extractChecklistIdFromListItem(oListItem) {
        var oCtx = oListItem && oListItem.getBindingContext && oListItem.getBindingContext();
        var oObject = oCtx && oCtx.getObject && oCtx.getObject();
        return ChecklistIdentity.extractChecklistId(oObject);
    }

    function extractChecklistDisplayIdFromListItem(oListItem) {
        var oCtx = oListItem && oListItem.getBindingContext && oListItem.getBindingContext();
        var oObject = oCtx && oCtx.getObject && oCtx.getObject();
        return ChecklistIdentity.extractChecklistDisplayId(oObject);
    }

    function extractSelectedRowIds(oEvent, oTable) {
        var aListItems = [];
        var aSelectedContexts = [];
        var aRowContexts = [];
        var aIds = [];
        if (oEvent && typeof oEvent.getParameter === "function") {
            aListItems = oEvent.getParameter("listItems") || [];
            if (!Array.isArray(aListItems) || !aListItems.length) {
                [
                    oEvent.getParameter("listItem"),
                    oEvent.getParameter("item"),
                    oEvent.getParameter("selectedItem")
                ].forEach(function (oCandidate) {
                    if (oCandidate) {
                        aListItems.push(oCandidate);
                    }
                });
            }
            aSelectedContexts = oEvent.getParameter("selectedContexts") || [];
            if (oEvent.getParameter("rowContext")) {
                aRowContexts.push(oEvent.getParameter("rowContext"));
            }
        }
        aIds = (aListItems || []).map(extractChecklistIdFromListItem);
        aIds = aIds.concat((aSelectedContexts || []).map(function (oCtx) {
            return ChecklistIdentity.extractChecklistId(oCtx && oCtx.getObject && oCtx.getObject());
        }));
        aIds = aIds.concat((aRowContexts || []).map(function (oCtx) {
            return ChecklistIdentity.extractChecklistId(oCtx && oCtx.getObject && oCtx.getObject());
        }));
        aIds = aIds.concat(
            ((oTable && oTable.getSelectedItems && oTable.getSelectedItems()) || []).map(extractChecklistIdFromListItem)
        );
        return ChecklistIdentity.normalizeChecklistIds(aIds);
    }

    function extractSelectedRowId(oEvent, oTable) {
        return extractSelectedRowIds(oEvent, oTable)[0] || "";
    }

    function extractSelectedRowDisplayId(oEvent, oTable) {
        var oListItem = null;
        var oSelectedItems = oTable && oTable.getSelectedItems ? (oTable.getSelectedItems() || []) : [];
        if (oEvent && typeof oEvent.getParameter === "function") {
            oListItem = oEvent.getParameter("listItem")
                || oEvent.getParameter("item")
                || oEvent.getParameter("selectedItem")
                || ((oEvent.getParameter("listItems") || [])[0]);
        }
        oListItem = oListItem || oSelectedItems[0] || null;
        return String((oListItem && extractChecklistDisplayIdFromListItem(oListItem)) || "").trim();
    }

    function resolveSelectedRowIdsFromInnerTable(oInnerTable) {
        var aSelectedItems = oInnerTable && oInnerTable.getSelectedItems ? (oInnerTable.getSelectedItems() || []) : [];
        return ChecklistIdentity.normalizeChecklistIds(aSelectedItems.map(extractChecklistIdFromListItem));
    }

    function resolveSelectedRowDisplayIdFromInnerTable(oInnerTable) {
        var aSelectedItems = oInnerTable && oInnerTable.getSelectedItems ? (oInnerTable.getSelectedItems() || []) : [];
        return String((aSelectedItems[0] && extractChecklistDisplayIdFromListItem(aSelectedItems[0])) || "").trim();
    }

    function applySelectionState(oController, aSelectedRowIds, sSelectedRowDisplayId, sSource, fnSelectionChanged) {
        var aIds = ChecklistIdentity.normalizeChecklistIds(aSelectedRowIds);
        if (typeof fnSelectionChanged !== "function") {
            return Promise.resolve();
        }
        return fnSelectionChanged({
            selectedRowId: aIds[0] || "",
            selectedRowDisplayId: String(sSelectedRowDisplayId || "").trim(),
            selectedRowIds: aIds,
            source: sSource || "selectionRuntime"
        });
    }

    function focusDomNode(oNode) {
        if (!oNode || typeof oNode.focus !== "function") {
            return false;
        }
        try {
            if (typeof oNode.getAttribute === "function" && !oNode.getAttribute("tabindex")) {
                oNode.setAttribute("tabindex", "-1");
            }
        } catch (_error) {
            // Ignore readonly attribute nodes.
        }
        SchedulingRuntime.restartTimer(0, function () {
            oNode.focus();
        }, 0);
        return true;
    }

    function focusDomSelector(sSelector) {
        if (typeof document === "undefined" || !sSelector) {
            return false;
        }
        return focusDomNode(document.querySelector(sSelector));
    }

    function focusSearchFilters(oController) {
        var oTarget = resolveSmartSearchButton(oController) || oController.byId("searchSmartFilterBar");
        if (!oTarget) {
            return focusDomSelector("[id$='searchSmartFilterBar-btnGo']")
                || focusDomSelector("[id$='searchSmartFilterBar']")
                || focusDomSelector("[id$='searchSmartFilterBar'] input");
        }
        if (FocusRuntime.focusSoon(oTarget)) {
            return true;
        }
        return focusDomSelector("[id$='searchSmartFilterBar-btnGo']")
            || focusDomSelector("[id$='searchSmartFilterBar']")
            || focusDomSelector("[id$='searchSmartFilterBar'] input");
    }

    function focusSearchResultsTable(oController) {
        var oInnerTable = resolveSearchInnerTable(oController);
        var aSelectedItems;
        var aItems;
        var oTarget;
        if (!oInnerTable) {
            return focusDomSelector("[id$='searchSmartTable']")
                || focusDomSelector(".searchResultsTable");
        }
        aSelectedItems = oInnerTable.getSelectedItems ? (oInnerTable.getSelectedItems() || []) : [];
        if (Array.isArray(aSelectedItems) && aSelectedItems.length) {
            oTarget = aSelectedItems[0];
        }
        if (!oTarget && oInnerTable.getItems) {
            aItems = oInnerTable.getItems() || [];
            if (Array.isArray(aItems) && aItems.length) {
                oTarget = aItems[0];
            }
        }
        if (!oTarget) {
            oTarget = oInnerTable;
        }
        if (FocusRuntime.focusSoon(oTarget)) {
            return true;
        }
        if (oTarget && typeof oTarget.getDomRef === "function" && focusDomNode(oTarget.getDomRef())) {
            return true;
        }
        return focusDomSelector("[id$='searchSmartTable']")
            || focusDomSelector(".searchResultsTable .sapMListTblRow")
            || focusDomSelector(".searchResultsTable .sapMListTbl");
    }

    function focusSearchToolbar(oController) {
        var oTarget = oController.byId("backendTopInput")
            || oController.byId("maxRowsInput")
            || oController.byId("smartTableCustomToolbar");
        if (!oTarget) {
            return focusDomSelector("[id$='backendTopInput-inner']")
                || focusDomSelector("[id$='maxRowsInput-inner']")
                || focusDomSelector("[id$='smartTableCustomToolbar']");
        }
        if (FocusRuntime.focusSoon(oTarget)) {
            return true;
        }
        if (oTarget && typeof oTarget.getDomRef === "function" && focusDomNode(oTarget.getDomRef())) {
            return true;
        }
        return focusDomSelector("[id$='backendTopInput-inner']")
            || focusDomSelector("[id$='backendTopInput']")
            || focusDomSelector("[id$='maxRowsInput-inner']")
            || focusDomSelector("[id$='maxRowsInput']")
            || focusDomSelector("[id$='smartTableCustomToolbar']")
            || focusDomSelector(".searchCreateActionBtn");
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
        aSelectedRowIds = resolveSelectedRowIdsFromInnerTable(oInnerTable);
        return Promise.resolve(
            applySelectionState(
                oController,
                aSelectedRowIds,
                resolveSelectedRowDisplayIdFromInnerTable(oInnerTable),
                "selectVisibleRows",
                fnSelectionChanged
            )
        ).then(function () {
            return {
                count: aSelectedRowIds.length,
                selectedRowIds: aSelectedRowIds
            };
        });
    }

    function clearSelection(oController, fnSelectionChanged) {
        var oInnerTable = resolveSearchInnerTable(oController);
        if (oInnerTable && oInnerTable.removeSelections) {
            oInnerTable.removeSelections(true);
        }
        return Promise.resolve(applySelectionState(oController, [], "", "clearSelection", fnSelectionChanged));
    }

    return {
        bindSearchTableRuntime: bindSearchTableRuntime,
        clearSelection: clearSelection,
        configureSearchResultTable: configureSearchResultTable,
        extractSelectedRowDisplayId: extractSelectedRowDisplayId,
        extractSelectedRowId: extractSelectedRowId,
        extractSelectedRowIds: extractSelectedRowIds,
        focusSearchFilters: focusSearchFilters,
        focusSearchResults: focusSearchResultsTable,
        focusSearchToolbar: focusSearchToolbar,
        resolveSearchInnerTable: resolveSearchInnerTable,
        resolveSmartSearchButton: resolveSmartSearchButton,
        selectVisibleRows: selectVisibleRows,
        syncSearchTableRuntimeState: syncSearchTableRuntimeState
    };
});
