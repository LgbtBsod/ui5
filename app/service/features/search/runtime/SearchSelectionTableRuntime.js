sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (ControllerViewStateRuntime, ModelStateRuntime, SearchUiContracts, JsRuntime, ModelContracts) {
    "use strict";

    var SEARCH_COLUMN_RULES = SearchUiContracts.COLUMN_RULES;
    var COMPACT_VIEWPORT_REM_MAX = SearchUiContracts.VIEWPORT.COMPACT_REM_MAX;
    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var TYPE_OBJECT = JsRuntime.TYPEOF.OBJECT;
    var METHODS = JsRuntime.METHODS;
    var STATE_MODEL = ModelContracts.MODELS.STATE;

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
        var oResultsShell = oController && oController.byId && oController.byId("searchResultsShell");
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

    return {
        bindSearchTableRuntime: bindSearchTableRuntime,
        configureSearchResultTable: configureSearchResultTable,
        syncSearchTableRuntimeState: syncSearchTableRuntimeState
    };
});
