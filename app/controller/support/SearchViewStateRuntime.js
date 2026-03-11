sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentRuntimeSupport",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/util/search/SearchMaxResults"
], function (JSONModel, ComponentRuntimeSupport, ControllerViewStateRuntime, ControllerModelRuntime, SearchMaxResults) {
    "use strict";

    function createViewModel(sScope) {
        return new JSONModel({
            busy: false,
            searchActionBusy: false,
            createActionBusy: false,
            hasSearched: false,
            hasRows: false,
            canExport: false,
            smartFilterReady: false,
            smartTableReady: false,
            smartFilterPersistencyKey: "pcctSmartFilterSession_" + String(sScope || "volatile"),
            smartTablePersistencyKey: "pcctSmartTableSession_" + String(sScope || "volatile"),
            filterHintVisible: false,
            filterHintType: "Information",
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
            scrollNavVisible: false,
            resultsToolbarNavVisible: false,
            analyticsRailBusy: false,
            analyticsRail: { total: 0, monthly: 0, failedChecks: 0, failedBarriers: 0, avgChecksRate: 0, avgBarriersRate: 0, refreshedAtText: "-", sourceText: "-" },
            analytics: {
                total: 0,
                failedChecks: 0,
                failedBarriers: 0,
                closedCount: 0,
                registeredCount: 0,
                source: "backend_aggregate",
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
        if (oSmartFilterBar && typeof oSmartFilterBar.isInitialised === "function") {
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
        if (typeof oInnerTable.setGrowing === "function") {
            oInnerTable.setGrowing(bGrowing);
        }
        if (typeof oInnerTable.setGrowingScrollToLoad === "function") {
            oInnerTable.setGrowingScrollToLoad(false);
        }
        if (typeof oInnerTable.setGrowingThreshold === "function") {
            oInnerTable.setGrowingThreshold(iThreshold);
        }
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
            return "Success";
        }
        if (sNorm === "REVIEW") {
            return "Warning";
        }
        return "Information";
    }

    return {
        createViewModel: createViewModel,
        resolveSearchUiSessionKey: resolveSearchUiSessionKey,
        isSmartControlsReady: isSmartControlsReady,
        syncSearchTableRequestWindow: syncSearchTableRequestWindow,
        formatHumanDateTime: ComponentRuntimeSupport.formatHumanDateTime,
        formatWorkflowStageText: formatWorkflowStageText,
        formatWorkflowStageState: formatWorkflowStageState
    };
});
