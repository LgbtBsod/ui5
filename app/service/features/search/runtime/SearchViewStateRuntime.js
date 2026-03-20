sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentFormattingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchMaxResults",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/SearchRuntimeContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiSemanticConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/JsRuntime"
], function (JSONModel, ComponentFormattingRuntime, ControllerViewStateRuntime, ControllerModelRuntime, SearchMaxResults, OperationSourceContracts, SearchRuntimeContracts, UiSemanticConstants, JsRuntime) {
    "use strict";

    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;
    var PERSISTENCY_PREFIXES = SearchRuntimeContracts.PERSISTENCY_PREFIXES;
    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;

    function createViewModel(sScope) {
        return new JSONModel({
            busy: false,
            searchActionBusy: false,
            createActionBusy: false,
            hasSearched: false,
            hasRows: false,
            resultCount: 0,
            canExport: false,
            smartFilterReady: false,
            smartTableReady: false,
            smartFilterPersistencyKey: PERSISTENCY_PREFIXES.SMART_FILTER_SESSION + String(sScope || "volatile"),
            smartTablePersistencyKey: PERSISTENCY_PREFIXES.SMART_TABLE_SESSION + String(sScope || "volatile"),
            filterHintVisible: false,
            filterHintType: UiSemanticConstants.MESSAGE_TYPE.INFORMATION,
            filterHintText: "",
            workflowStage: "DISCOVER",
            lastUpdatedAt: "-",
            bootstrapBusy: false,
            analyticsBusy: false,
            analyticsError: "",
            tableBusy: false,
            exportBusy: false,
            hasSelection: false,
            selectionCount: 0,
            selectedRowIds: [],
            canCopy: false,
            canDelete: false,
            selectedRowId: "",
            selectedRowDisplayId: "",
            analyticsRailBusy: false,
            analyticsRail: { total: 0, monthly: 0, failedChecks: 0, failedBarriers: 0, avgChecksRate: 0, avgBarriersRate: 0, refreshedAtText: "-", sourceText: "-" },
            analytics: {
                total: 0,
                failedChecks: 0,
                failedBarriers: 0,
                closedCount: 0,
                registeredCount: 0,
                source: SEARCH_SOURCES.BACKEND_AGGREGATE,
                avgChecksRate: 0,
                avgBarriersRate: 0,
                refreshedAt: "-",
                charts: {
                    failedChecksByProfession: [],
                    failedBarriersByProfession: [],
                    failedChecksByLpc: [],
                    failedBarriersByLpc: []
                },
                hasCharts: {
                    failedChecksByProfession: false,
                    failedBarriersByProfession: false,
                    failedChecksByLpc: false,
                    failedBarriersByLpc: false
                }
            }
        });
    }

    function resolveSearchUiSessionKey() {
        var sKey = "";
        try {
            sKey = window.sessionStorage.getItem("pcct_search_ui_session") || "";
            if (!sKey) {
                sKey = "S" + Math.random().toString(36).slice(2) + Date.now().toString(36);
                window.sessionStorage.setItem("pcct_search_ui_session", sKey);
            }
        } catch (e) {
            sKey = "volatile";
        }
        return sKey;
    }

    function isSmartControlsReady(oController) {
        var oSmartFilterBar = oController.byId("searchSmartFilterBar");
        var bSmartFilterReady = !!ControllerViewStateRuntime.get(oController, "/smartFilterReady", false);
        var bSmartTableReady = !!ControllerViewStateRuntime.get(oController, "/smartTableReady", false);

        if (!bSmartFilterReady || !bSmartTableReady) {
            return false;
        }
        if (oSmartFilterBar && typeof oSmartFilterBar.isInitialised === TYPE_FUNCTION) {
            return !!oSmartFilterBar.isInitialised();
        }
        return true;
    }

    function resolveVisibleCap(mStateData) {
        var sValue;
        var iParsed;

        if (SearchMaxResults && typeof SearchMaxResults.resolveGrowingPageSize === "function") {
            return SearchMaxResults.resolveGrowingPageSize(mStateData);
        }
        if (SearchMaxResults && typeof SearchMaxResults.resolveMaxResults === "function") {
            return SearchMaxResults.resolveMaxResults(mStateData);
        }

        sValue = String((mStateData || {}).growingPageSize || (mStateData || {}).searchMaxResults || "").trim();
        iParsed = Number(sValue);
        if (!sValue || !isFinite(iParsed) || iParsed <= 0) {
            return 0;
        }
        return Math.max(1, Math.min(9999, Math.floor(iParsed)));
    }

    function syncSearchTableRequestWindow(oController) {
        var oSmartTable = oController.byId("searchSmartTable");
        var oInnerTable = oSmartTable && oSmartTable.getTable && oSmartTable.getTable();
        var oStateModel = ControllerModelRuntime.state(oController);
        var oStateData = (oStateModel && oStateModel.getData && oStateModel.getData()) || {};
        var iVisibleCap = resolveVisibleCap(oStateData) || 100;
        var iBackendTop = SearchMaxResults.resolveSearchFetchLimit(oStateData);
        var iThreshold = iVisibleCap;
        var bGrowing = true;

        if (!oInnerTable) {
            return;
        }
        if (iBackendTop > 0 && iBackendTop < iThreshold) {
            iThreshold = iBackendTop;
            bGrowing = false;
        }
        if (typeof oInnerTable.setGrowing === TYPE_FUNCTION) {
            oInnerTable.setGrowing(bGrowing);
        }
        if (typeof oInnerTable.setGrowingScrollToLoad === TYPE_FUNCTION) {
            oInnerTable.setGrowingScrollToLoad(false);
        }
        if (typeof oInnerTable.setGrowingThreshold === TYPE_FUNCTION) {
            oInnerTable.setGrowingThreshold(iThreshold);
        }
    }

    function setSmartTableReady(oController, bValue) {
        ControllerViewStateRuntime.set(oController, "/smartTableReady", !!bValue);
    }

    function setTableBusy(oController, bValue) {
        ControllerViewStateRuntime.set(oController, "/tableBusy", !!bValue);
    }

    function readStateData(oController) {
        var oStateModel = ControllerModelRuntime.state(oController);
        return (oStateModel && typeof oStateModel.getData === TYPE_FUNCTION && oStateModel.getData()) || {};
    }

    function formatWorkflowStageText(oBundle, sStage) {
        var mFallbackText = {
            ANALYZE: "Analyze",
            REVIEW: "Review",
            DISCOVER: "Discover"
        };
        var mStageKey = {
            ANALYZE: "workflowStageAnalyze",
            REVIEW: "workflowStageReview"
        };
        var sStageNorm = String(sStage || "").toUpperCase() || "DISCOVER";
        var sKey = mStageKey[sStageNorm] || "workflowStageDiscover";
        if (oBundle && oBundle.hasText && oBundle.hasText(sKey)) {
            return oBundle.getText(sKey);
        }
        return mFallbackText[sStageNorm] || mFallbackText.DISCOVER;
    }

    function formatWorkflowStageState(sStage) {
        var sNorm = String(sStage || "").toUpperCase();
        if (sNorm === "ANALYZE") {
            return UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS;
        }
        if (sNorm === "REVIEW") {
            return UiSemanticConstants.OBJECT_STATUS_STATE.WARNING;
        }
        return UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION;
    }

    return {
        createViewModel: createViewModel,
        resolveSearchUiSessionKey: resolveSearchUiSessionKey,
        isSmartControlsReady: isSmartControlsReady,
        readStateData: readStateData,
        setSmartTableReady: setSmartTableReady,
        setTableBusy: setTableBusy,
        syncSearchTableRequestWindow: syncSearchTableRequestWindow,
        formatHumanDateTime: ComponentFormattingRuntime.formatHumanDateTime,
        formatWorkflowStageText: formatWorkflowStageText,
        formatWorkflowStageState: formatWorkflowStageState
    };
});
