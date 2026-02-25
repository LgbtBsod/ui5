sap.ui.define([
    "sap_ui5/controller/Base.controller",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageToast",
    "sap/m/MessageBox",
    "sap_ui5/service/SmartSearchAdapter",
    "sap_ui5/service/usecase/SearchApplicationService",
    "sap_ui5/service/usecase/WorkflowAnalyticsUseCase",
    "sap_ui5/service/usecase/SearchActionUseCase",
    "sap_ui5/service/usecase/SearchUiFlowUseCase",
    "sap_ui5/service/usecase/SearchIntentUseCase",
    "sap_ui5/service/usecase/SearchLoadFilterUseCase",
    "sap_ui5/service/usecase/SearchAnalyticsExportUseCase",
    "sap_ui5/service/usecase/SearchAnalyticsDialogExportFlowUseCase",
    "sap_ui5/service/usecase/SearchPresentationUseCase",
    "sap_ui5/util/ExcelExport",
    "sap_ui5/util/FlowCoordinator",
    "sap_ui5/util/SearchWorkflowOrchestrator",
    "sap_ui5/util/SearchSmartControlCoordinator"
], function (BaseController, JSONModel, MessageToast, MessageBox, SmartSearchAdapter, SearchApplicationService, WorkflowAnalyticsUseCase, SearchActionUseCase, SearchUiFlowUseCase, SearchIntentUseCase, SearchLoadFilterUseCase, SearchAnalyticsExportUseCase, SearchAnalyticsDialogExportFlowUseCase, SearchPresentationUseCase, ExcelExport, FlowCoordinator, SearchWorkflowOrchestrator, SearchSmartControlCoordinator) {
    "use strict";

    return BaseController.extend("sap_ui5.controller.Search", {

        onInit: function () {
            var oStateModel = this.getModel("state");
            var oLayoutModel = this.getModel("layout");
            var oViewModel = new JSONModel({
                hasActiveFilters: false,
                hasSelection: false,
                hasSearched: false,
                resultSummary: "",
                workflowStage: "DISCOVER",
                kpiVisible: 0,
                kpiTotal: 0,
                kpiFailedChecks: 0,
                kpiFailedBarriers: 0,
                kpiHealthy: 0,
                lastUpdatedAt: "",
                smartFilterReady: true,
                smartTableReady: true,
                useSmartControls: true,
                smartControlsReason: "",
                smartControlError: "",
                analytics: {
                    total: 0,
                    failedChecks: 0,
                    failedBarriers: 0,
                    healthy: 0,
                    closedCount: 0,
                    registeredCount: 0,
                    avgChecksRate: 0,
                    avgBarriersRate: 0,
                    refreshedAt: "",
                    source: "fallback"
                },
                analyticsBusy: false,
                analyticsError: ""
            });

            this._iSearchDebounceMs = 180;
            this._iSearchTimer = null;

            this.applyStoredTheme();
            this.setModel(oViewModel, "view");
            this.getView().setModel(this.getOwnerComponent().getModel("mainService"));

            oLayoutModel.setProperty("/smartFilter/fields", SmartSearchAdapter.getSmartFilterConfig().fields);
            oLayoutModel.setProperty("/smartTable/columns", SmartSearchAdapter.getSmartTableConfig().columns);

            ["/filterId", "/filterLpc", "/filterFailedChecks", "/filterFailedBarriers"].forEach(function (sPath) {
                oStateModel.bindProperty(sPath).attachChange(this._onFilterChanged, this);
            }.bind(this));
            ["/mainServiceMetadataOk", "/mainServiceMetadataError"].forEach(function (sPath) {
                oStateModel.bindProperty(sPath).attachChange(this._syncSmartControlAvailability, this);
            }.bind(this));

            this.attachRouteMatched("search", this._onSearchMatched);
            this._syncSmartControlAvailability();
            this._bootstrapSmartControls();
            this._updateFilterState();
            this._updateResultSummary();

            var oDataModel = this.getModel("data");
            ["/visibleCheckLists", "/checkLists"].forEach(function (sPath) {
                oDataModel.bindProperty(sPath).attachChange(this._updateResultSummary, this);
            }.bind(this));
        },




        _getViewModel: function () {
            return this.getView().getModel("view");
        },

        _syncSmartControlAvailability: function () {
            SearchSmartControlCoordinator.syncAvailability({
                stateModel: this.getModel("state"),
                viewModel: this._getViewModel(),
                unavailableText: this.getResourceBundle().getText("smartControlsUnavailable"),
                bootstrap: this._bootstrapSmartControls.bind(this)
            });
        },

        _isSmartControlsEnabled: function () {
            return SearchSmartControlCoordinator.isEnabled(this._getViewModel());
        },

        _setSmartControlsEnabled: function (bEnabled, sError) {
            SearchSmartControlCoordinator.setEnabled(this._getViewModel(), bEnabled, sError);
        },

        _bootstrapSmartControls: async function () {
            return SearchSmartControlCoordinator.bootstrap({
                smartTable: this.byId("searchSmartTable"),
                setEnabled: this._setSmartControlsEnabled.bind(this),
                onReady: this._onSmartTableReady.bind(this)
            });
        },

        _onSmartTableReady: function () {
            this._oSmartInnerTable = SearchSmartControlCoordinator.wireInnerTable({
                smartTable: this.byId("searchSmartTable"),
                setEnabled: this._setSmartControlsEnabled.bind(this),
                onSelectionChange: this._onSmartTableSelectionChange.bind(this)
            });
        },

        _extractChecklistIdFromObject: function (oObject) {
            return SearchSmartControlCoordinator.extractChecklistId(oObject);
        },

        _selectChecklistById: async function (sId) {
            if (!sId) {
                MessageToast.show(this.getResourceBundle().getText("checklistIdMissing"));
                return;
            }

            var bCanNavigate = await this._confirmNavigationFromDirty();
            if (!bCanNavigate) {
                return;
            }

            var oChecklist = await SearchApplicationService.getChecklistById(sId).catch(function () { return { root: { id: sId } }; });

            this.getModel("selected").setData(oChecklist);
            this._getViewModel().setProperty("/hasSelection", true);
            this.getModel("state").setProperty("/layout", "TwoColumnsMidExpanded");
            this.navTo("detail", { id: sId });
        },

        _onSmartTableSelectionChange: function (oEvent) {
            var sId = SearchSmartControlCoordinator.extractChecklistIdFromSelectionEvent(oEvent);
            this._selectChecklistById(sId);
        },

        onSmartTableInitialise: function () {
            this._onSmartTableReady();
        },

        onSmartFilterChanged: function () {
            this._updateFilterState();
        },

        onBeforeSmartTableRebind: function (oEvent) {
            if (!this._isSmartControlsEnabled()) {
                return;
            }

            var oState = this.getModel("state");
            SearchSmartControlCoordinator.applyRebindParams({
                bindingParams: oEvent.getParameter("bindingParams") || {},
                state: {
                    filterId: oState.getProperty("/filterId"),
                    filterLpc: oState.getProperty("/filterLpc"),
                    filterFailedChecks: oState.getProperty("/filterFailedChecks"),
                    filterFailedBarriers: oState.getProperty("/filterFailedBarriers"),
                    searchMaxResults: oState.getProperty("/searchMaxResults")
                },
                onDataReceived: function (oDataEvent) {
                    this._updateSmartTableAnalytics(oDataEvent && oDataEvent.getParameter("data"));
                }.bind(this)
            });
        },

        _rebindSmartTable: function () {
            SearchSmartControlCoordinator.rebindOrFallback({
                enabled: this._isSmartControlsEnabled(),
                smartTable: this.byId("searchSmartTable"),
                fallbackSearch: this._executeSearch.bind(this)
            });
        },

        _onSearchMatched: function () {
            var oStateModel = this.getModel("state");

            oStateModel.setProperty("/layout", "OneColumn");
            oStateModel.setProperty("/mode", "READ");
            this._syncSmartControlAvailability();
            this._updateResultSummary();
        },

        _applySearchMetrics: function (aRows, iTotalOverride) {
            var oViewModel = this._getViewModel();
            var oMetrics = SearchWorkflowOrchestrator.buildMetrics(aRows, iTotalOverride);
            var oKpi = oMetrics.kpi;

            oViewModel.setProperty("/kpiVisible", oKpi.visible);
            oViewModel.setProperty("/kpiTotal", oKpi.total);
            oViewModel.setProperty("/kpiFailedChecks", oKpi.failedChecks);
            oViewModel.setProperty("/kpiFailedBarriers", oKpi.failedBarriers);
            oViewModel.setProperty("/kpiHealthy", oKpi.healthy);
            oViewModel.setProperty("/workflowStage", oKpi.workflowStage);
            oViewModel.setProperty("/lastUpdatedAt", new Date().toLocaleTimeString());
            oViewModel.setProperty("/resultSummary", this._buildResultSummaryText(oMetrics.visible, oMetrics.total));
        },

        _buildResultSummaryText: function (iVisible, iTotal) {
            return this.getResourceBundle().getText("resultSummary", [iVisible, iTotal]);
        },

        _updateSmartTableAnalytics: function (oRawData) {
            var aRows = SearchWorkflowOrchestrator.normalizeRows(oRawData);
            var iTotal = Number(oRawData && (oRawData.__count || oRawData["@odata.count"]));
            this._applySearchMetrics(aRows, iTotal);
        },

        _onFilterChanged: function () {
            this._updateFilterState();

            // By product decision search starts only on explicit Search button.
        },

        _scheduleSearch: function () {
            if (this._iSearchTimer) {
                clearTimeout(this._iSearchTimer);
            }

            this._iSearchTimer = setTimeout(function () {
                this._executeSearch();
                this._iSearchTimer = null;
            }.bind(this), this._iSearchDebounceMs);
        },

        _buildFilterPayload: function () {
            var oStateModel = this.getModel("state");
            var sRawMax = String(oStateModel.getProperty("/searchMaxResults") || "").trim();
            var iMax = sRawMax ? Number(sRawMax) : null;
            if (iMax !== null) {
                iMax = Math.max(1, Math.min(9999, iMax || 0));
            }

            return {
                filterId: oStateModel.getProperty("/filterId"),
                filterLpc: oStateModel.getProperty("/filterLpc"),
                filterFailedChecks: oStateModel.getProperty("/filterFailedChecks"),
                filterFailedBarriers: oStateModel.getProperty("/filterFailedBarriers"),
                maxResults: iMax
            };
        },

        _updateFilterState: function () {
            var mPayload = this._buildFilterPayload();
            var bHasFilters = Boolean((mPayload.filterId || "").trim())
                || Boolean(mPayload.filterLpc)
                || mPayload.filterFailedChecks !== "ALL"
                || mPayload.filterFailedBarriers !== "ALL";

            this._getViewModel().setProperty("/hasActiveFilters", bHasFilters);
        },

        _updateResultSummary: function () {
            var oDataModel = this.getModel("data");
            var aRows = oDataModel.getProperty("/visibleCheckLists") || [];
            var iTotal = (oDataModel.getProperty("/checkLists") || []).length;

            this._applySearchMetrics(aRows, iTotal);
        },

        onWorkflowQuickAction: function (oEvent) {
            var sAction = oEvent.getSource().data("action");
            switch (sAction) {
                case "CREATE":
                    this.onCreate();
                    break;
                case "REFRESH":
                    this._rebindSmartTable();
                    this._getViewModel().setProperty("/workflowStage", "REVIEW");
                    break;
                case "RESET":
                    this.onResetFilters();
                    break;
                default:
                    break;
            }
        },

        _executeSearch: function () {
            var oDataModel = this.getModel("data");
            var oStateModel = this.getModel("state");
            var mPayload = this._buildFilterPayload();
            var sSearchMode = oStateModel.getProperty("/searchMode") || "EXACT";

            return this.runWithStateFlag(oStateModel, "/isLoading", function () {
                return SearchApplicationService.runSearch(mPayload, sSearchMode, oDataModel.getProperty("/checkLists") || []).then(function (aFiltered) {
                    oDataModel.setProperty("/visibleCheckLists", aFiltered);
                    this._updateResultSummary();
                }.bind(this));
            }.bind(this));
        },

        onSelect: async function (oEvent) {
            this._getViewModel().setProperty("/hasSelection", true);
            var sId = SearchUiFlowUseCase.extractIdFromListSelectionEvent(oEvent);
            await this._selectChecklistById(sId);
        },



        _confirmNavigationFromDirty: function () {
            return FlowCoordinator.confirmUnsavedAndHandle(this, function () {
                return Promise.resolve(false);
            }).then((sDecision) => {
                return sDecision !== "CANCEL";
            });
        },

        onCreate: function () {
            this._confirmNavigationFromDirty().then(function (bCanNavigate) {
                if (!bCanNavigate) {
                    return;
                }
                this.getModel("state").setProperty("/objectAction", "CREATE");
                this.getModel("state").setProperty("/layout", "TwoColumnsMidExpanded");
                // Unified single object card: detail route handles create mode too.
                this.navTo("detail", { id: "__create" });
            }.bind(this));
        },

        onCopy: function () {
            var oSelected = this.getModel("selected").getData() || {};
            var sId = SearchActionUseCase.extractSelectedChecklistId(oSelected);

            if (!sId) {
                MessageToast.show(this.getResourceBundle().getText("nothingToCopy"));
                return;
            }

            this._confirmNavigationFromDirty().then(function (bCanNavigate) {
                if (!bCanNavigate) {
                    return;
                }
                this.getModel("state").setProperty("/objectAction", "COPY");
                this.getModel("state").setProperty("/layout", "TwoColumnsMidExpanded");
                // Unified single object card.
                this.navTo("detail", { id: sId });
            }.bind(this));
        },

        onDelete: function () {
            var oSelected = this.getModel("selected").getData() || {};
            var sId = SearchActionUseCase.extractSelectedChecklistId(oSelected);
            var oBundle = this.getResourceBundle();

            if (!sId) {
                MessageToast.show(oBundle.getText("nothingToDelete"));
                return;
            }

            this.runWithStateFlag(this.getModel("state"), "/isLoading", function () {
                return SearchApplicationService.deleteChecklistAndReload(sId).then(function (aUpdated) {
                    this.getModel("data").setProperty("/checkLists", aUpdated);
                    this._rebindSmartTable();
                    this.getModel("selected").setData({});
                    this.getView().getModel("view").setProperty("/hasSelection", false);
                    MessageToast.show(oBundle.getText("deleted"));
                }.bind(this)).catch(function (oError) {
                    MessageToast.show(oBundle.getText("deleteFailed", [((oError && oError.message) || "Unknown error")]));
                });
            }.bind(this));
        },

        onSmartSearch: function () {
            SearchIntentUseCase.markSearchedAndRebind(this._getViewModel(), this._rebindSmartTable.bind(this));
        },

        onSearch: function () {
            SearchIntentUseCase.markSearchedAndRebind(this._getViewModel(), this._rebindSmartTable.bind(this));
        },

        onStatusFilterPress: function (oEvent) {
            var oSource = oEvent.getSource();
            var sFilterPath = oSource.data("filterPath");
            var sFilterValue = oSource.data("filterValue");

            SearchIntentUseCase.applyStatusFilter(this.getModel("state"), sFilterPath, sFilterValue, this._rebindSmartTable.bind(this));
        },





        _buildAnalyticsPayload: function () {
            var mPayload = this._buildFilterPayload();
            return SearchActionUseCase.buildAnalyticsPayload(mPayload);
        },

        _loadWorkflowAnalytics: function () {
            var oViewModel = this._getViewModel();
            var mPayload = this._buildAnalyticsPayload();
            var sSearchMode = this.getModel("state").getProperty("/searchMode") || "EXACT";
            var aFallback = this.getModel("data").getProperty("/checkLists") || [];

            oViewModel.setProperty("/analyticsBusy", true);
            oViewModel.setProperty("/analyticsError", "");

            return WorkflowAnalyticsUseCase.loadProcessAnalytics(mPayload, sSearchMode, aFallback).then(function (oAnalytics) {
                oViewModel.setProperty("/analytics", oAnalytics || {});
            }).catch(function (oError) {
                oViewModel.setProperty("/analyticsError", (oError && oError.message) || "");
            }).finally(function () {
                oViewModel.setProperty("/analyticsBusy", false);
            });
        },

        onOpenWorkflowAnalytics: function () {
            var oDialog = this.byId("workflowAnalyticsDialog");
            SearchAnalyticsDialogExportFlowUseCase.openAnalyticsDialog(oDialog, this._loadWorkflowAnalytics.bind(this));
        },

        onCloseWorkflowAnalytics: function () {
            var oDialog = this.byId("workflowAnalyticsDialog");
            SearchAnalyticsDialogExportFlowUseCase.closeAnalyticsDialog(oDialog);
        },

        _collectScreenRowsForExport: function () {
            var aRows = this.getModel("data").getProperty("/visibleCheckLists") || [];
            return SearchActionUseCase.buildExportRowsFromVisible(aRows);
        },

        _runExport: function (sEntity) {
            var mPayload = this._buildFilterPayload();
            var sSearchMode = this.getModel("state").getProperty("/searchMode") || "EXACT";
            var oBundle = this.getResourceBundle();

            return SearchAnalyticsDialogExportFlowUseCase.runExportFlow({
                runWithLoading: function (fnTask) {
                    return this.runWithStateFlag(this.getModel("state"), "/isLoading", fnTask);
                }.bind(this),
                buildExportPromise: function () {
                    return SearchAnalyticsExportUseCase.buildExportPromise(
                        sEntity,
                        this._collectScreenRowsForExport.bind(this),
                        function () { return SearchApplicationService.exportRows(sEntity, mPayload, sSearchMode); }
                    );
                }.bind(this),
                onEmpty: function () {
                    MessageToast.show(oBundle.getText("exportEmpty"));
                },
                onSuccess: function (aRows) {
                    ExcelExport.download("checklist_" + sEntity + "_" + Date.now(), aRows);
                    MessageToast.show(oBundle.getText("exportDone", [aRows.length]));
                },
                onError: function (oError) {
                    MessageToast.show(oBundle.getText("exportFailed", [((oError && oError.message) || "Unknown error")]));
                }
            });
        },

        onExportMenuDefault: function () {
            this._runExport("screen");
        },

        onExportMenuAction: function (oEvent) {
            var sEntity = SearchUiFlowUseCase.resolveExportEntityFromMenuEvent(oEvent, "screen");
            this._runExport(sEntity);
        },

        onExportReport: function (oEvent) {
            var sEntity = oEvent.getSource().data("entity") || "screen";
            this._runExport(sEntity);
        },

        onRetryLoad: function () {
            var oStateModel = this.getModel("state");
            var oDataModel = this.getModel("data");

            return SearchLoadFilterUseCase.runRetryLoad({
                resetLoadError: function () {
                    oStateModel.setProperty("/loadError", false);
                    oStateModel.setProperty("/loadErrorMessage", "");
                },
                runWithLoading: function (fnTask) {
                    return this.runWithStateFlag(oStateModel, "/isLoading", fnTask);
                }.bind(this),
                getCheckLists: SearchApplicationService.getCheckLists,
                applyLoadedRows: function (aCheckLists) {
                    oDataModel.setProperty("/checkLists", aCheckLists);
                    oDataModel.setProperty("/visibleCheckLists", aCheckLists);
                    this._updateResultSummary();
                }.bind(this),
                applyLoadError: function (oError) {
                    oStateModel.setProperty("/loadError", true);
                    oStateModel.setProperty("/loadErrorMessage", (oError && oError.message) || "");
                }
            });
        },

        onResetFilters: function () {
            SearchLoadFilterUseCase.resetFilters(this.getModel("state"), this._getViewModel());
            this._rebindSmartTable();
        },


        onMaxRowsChange: function () {
            var oState = this.getModel("state");
            var sNormalized = SearchPresentationUseCase.normalizeMaxRowsInput(oState.getProperty("/searchMaxResults"));
            oState.setProperty("/searchMaxResults", sNormalized);
        },

        onSearchModeToggle: function (oEvent) {
            var bLoose = oEvent.getParameter("state");
            SearchLoadFilterUseCase.applySearchMode(this.getModel("state"), this._getViewModel(), bLoose);
            this._rebindSmartTable();
        },

        onExit: function () {
            if (this._iSearchTimer) {
                clearTimeout(this._iSearchTimer);
                this._iSearchTimer = null;
            }
        },


        formatOverallResultText: function (vResult, sLifecycleStatus) {
            return SearchPresentationUseCase.formatOverallResultText(vResult, sLifecycleStatus, this.getResourceBundle());
        },

        formatOverallResultState: function (vResult, sLifecycleStatus) {
            return SearchPresentationUseCase.formatOverallResultState(vResult, sLifecycleStatus);
        },

        formatStatus: function (sStatus) {
            return SearchPresentationUseCase.formatStatus(sStatus);
        }
    });
});
