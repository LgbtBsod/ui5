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
    "sap_ui5/service/usecase/SearchRetryLoadPresentationUseCase",
    "sap_ui5/service/usecase/SearchAnalyticsExportUseCase",
    "sap_ui5/service/usecase/SearchAnalyticsDialogExportFlowUseCase",
    "sap_ui5/service/usecase/SearchPresentationUseCase",
    "sap_ui5/service/usecase/SearchSelectionNavigationUseCase",
    "sap_ui5/service/usecase/SearchSmartFilterFlowUseCase",
    "sap_ui5/service/usecase/SearchWorkflowAnalyticsDialogUseCase",
    "sap_ui5/service/usecase/SearchExportOrchestrationUseCase",
    "sap_ui5/service/usecase/SearchToolbarActionStateUseCase",
    "sap_ui5/service/usecase/SearchNavigationIntentUseCase",
    "sap_ui5/service/usecase/SearchDeleteOrchestrationUseCase",
    "sap_ui5/service/usecase/SearchActionMessagePresentationUseCase",
    "sap_ui5/service/usecase/SearchSelectionHydrationUseCase",
    "sap_ui5/service/usecase/SearchOpenDetailGuardUseCase",
    "sap_ui5/service/usecase/SearchCreateCopyNavigationGuardUseCase",
    "sap_ui5/service/usecase/SearchExportIntentGuardUseCase",
    "sap_ui5/service/usecase/SearchRetryMessagePresentationUseCase",
    "sap_ui5/service/usecase/SearchSummaryPresentationUseCase",
    "sap_ui5/service/usecase/SearchEmptyStatePresentationUseCase",
    "sap_ui5/util/ExcelExport",
    "sap_ui5/util/FlowCoordinator",
    "sap_ui5/util/SearchWorkflowOrchestrator",
    "sap_ui5/util/SearchSmartControlCoordinator"
], function (BaseController, JSONModel, MessageToast, MessageBox, SmartSearchAdapter, SearchApplicationService, WorkflowAnalyticsUseCase, SearchActionUseCase, SearchUiFlowUseCase, SearchIntentUseCase, SearchLoadFilterUseCase, SearchRetryLoadPresentationUseCase, SearchAnalyticsExportUseCase, SearchAnalyticsDialogExportFlowUseCase, SearchPresentationUseCase, SearchSelectionNavigationUseCase, SearchSmartFilterFlowUseCase, SearchWorkflowAnalyticsDialogUseCase, SearchExportOrchestrationUseCase, SearchToolbarActionStateUseCase, SearchNavigationIntentUseCase, SearchDeleteOrchestrationUseCase, SearchActionMessagePresentationUseCase, SearchSelectionHydrationUseCase, SearchOpenDetailGuardUseCase, SearchCreateCopyNavigationGuardUseCase, SearchExportIntentGuardUseCase, SearchRetryMessagePresentationUseCase, SearchSummaryPresentationUseCase, SearchEmptyStatePresentationUseCase, ExcelExport, FlowCoordinator, SearchWorkflowOrchestrator, SearchSmartControlCoordinator) {
    "use strict";

    return BaseController.extend("sap_ui5.controller.Search", {

        onInit: function () {
            var oStateModel = this.getModel("state");
            var oLayoutModel = this.getModel("layout");
            var oViewModel = new JSONModel({
                hasActiveFilters: false,
                hasSelection: false,
                canCopy: false,
                canDelete: false,
                canExport: false,
                canRetryLoad: true,
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
                analyticsError: "",
                noDataText: "",
                emptyStateKind: "default_empty"
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
                onSelectionChange: this._onSmartTableSelectionChange.bind(this),
                onItemPress: this._onSmartTableItemPress.bind(this)
            });
            this._applyEmptyStatePresentation();
        },

        _extractChecklistIdFromObject: function (oObject) {
            return SearchSmartControlCoordinator.extractChecklistId(oObject);
        },

        _loadSelectedChecklistById: function (sId) {
            return SearchSelectionHydrationUseCase.runSelectionHydration({
                id: sId,
                selectedModel: this.getModel("selected"),
                viewModel: this._getViewModel(),
                loadChecklistById: SearchApplicationService.getChecklistById
            }).then(function (oResult) {
                return (oResult && oResult.checklist) || null;
            });
        },

        _openDetailById: function (sId) {
            return SearchOpenDetailGuardUseCase.runOpenDetailFlow({
                id: sId,
                confirmNavigation: this._confirmNavigationFromDirty.bind(this),
                buildIntent: function (sIntentId) {
                    return SearchNavigationIntentUseCase.buildOpenDetailIntent({ id: sIntentId });
                },
                applyIntent: function (mIntent) {
                    return SearchNavigationIntentUseCase.applyIntent({
                        intent: mIntent,
                        stateModel: this.getModel("state"),
                        navTo: this.navTo.bind(this)
                    });
                }.bind(this)
            }).then(function (oResult) {
                if (oResult && oResult.reason === "missing_id") {
                    SearchActionMessagePresentationUseCase.presentMissingChecklistId({
                        bundle: this.getResourceBundle(),
                        showToast: MessageToast.show
                    });
                }
                return oResult;
            }.bind(this));
        },

        _onSmartTableSelectionChange: function (oEvent) {
            var sId = SearchSmartControlCoordinator.extractChecklistIdFromSelectionEvent(oEvent);
            this._loadSelectedChecklistById(sId);
        },

        _onSmartTableItemPress: function (oEvent) {
            var sId = SearchSmartControlCoordinator.extractChecklistIdFromSelectionEvent(oEvent);
            this._loadSelectedChecklistById(sId).then(function () {
                return this._openDetailById(sId);
            }.bind(this));
        },

        onSmartTableInitialise: function () {
            this._onSmartTableReady();
        },

        _syncStateFiltersFromSmartFilter: function () {
            if (!this._isSmartControlsEnabled()) {
                return;
            }

            SearchSmartFilterFlowUseCase.syncStateFiltersFromSmartFilter({
                smartFilterBar: this.byId("searchSmartFilterBar"),
                stateModel: this.getModel("state")
            });
        },

        onSmartFilterChanged: function () {
            this._syncStateFiltersFromSmartFilter();
            this._updateFilterState();
        },

        onBeforeSmartTableRebind: function (oEvent) {
            if (!this._isSmartControlsEnabled()) {
                return;
            }

            SearchSmartFilterFlowUseCase.prepareRebindParams({
                bindingParams: oEvent.getParameter("bindingParams") || {},
                state: {
                    filterId: this.getModel("state").getProperty("/filterId"),
                    filterLpc: this.getModel("state").getProperty("/filterLpc"),
                    filterFailedChecks: this.getModel("state").getProperty("/filterFailedChecks"),
                    filterFailedBarriers: this.getModel("state").getProperty("/filterFailedBarriers"),
                    searchMaxResults: this.getModel("state").getProperty("/searchMaxResults"),
                    searchMode: this.getModel("state").getProperty("/searchMode") || "EXACT"
                },
                onDataReceived: function (oDataEvent) {
                    this._updateSmartTableAnalytics(oDataEvent && oDataEvent.getParameter("data"));
                }.bind(this),
                applyRebindParams: SearchSmartControlCoordinator.applyRebindParams
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
            this._syncSearchSelectionState();
            this._updateResultSummary();
            this._applyEmptyStatePresentation();
        },

        _applySearchMetrics: function (aRows, iTotalOverride) {
            var oMetrics = SearchWorkflowOrchestrator.buildMetrics(aRows, iTotalOverride);
            return SearchSummaryPresentationUseCase.applySummaryPresentation({
                viewModel: this._getViewModel(),
                bundle: this.getResourceBundle(),
                kpi: oMetrics.kpi,
                visible: oMetrics.visible,
                total: oMetrics.total,
                lastUpdatedAt: new Date().toLocaleTimeString()
            });
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

        _hasSmartFilters: function () {
            var oSmartFilterBar = this.byId("searchSmartFilterBar");
            if (!oSmartFilterBar || !this._isSmartControlsEnabled() || !oSmartFilterBar.getFiltersWithValues) {
                return false;
            }
            var aFilters = oSmartFilterBar.getFiltersWithValues(true) || [];
            return aFilters.length > 0;
        },

        _updateFilterState: function () {
            if (this._isSmartControlsEnabled()) {
                this._getViewModel().setProperty("/hasActiveFilters", this._hasSmartFilters());
                return;
            }

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
            this._applyEmptyStatePresentation();
        },

        _applyEmptyStatePresentation: function () {
            return SearchEmptyStatePresentationUseCase.applyEmptyStatePresentation({
                viewModel: this._getViewModel(),
                dataModel: this.getModel("data"),
                bundle: this.getResourceBundle(),
                table: this._oSmartInnerTable || this.byId("searchSmartTable") || this.byId("searchFallbackTable"),
                unknownFallbackKey: "noDataDefault"
            });
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

        onFallbackSelectionChange: function (oEvent) {
            var sId = SearchUiFlowUseCase.extractIdFromListSelectionEvent(oEvent);
            this._loadSelectedChecklistById(sId);
        },

        onFallbackItemPress: function (oEvent) {
            var sId = SearchUiFlowUseCase.extractIdFromListSelectionEvent(oEvent);
            this._loadSelectedChecklistById(sId).then(function () {
                return this._openDetailById(sId);
            }.bind(this));
        },



        _confirmNavigationFromDirty: function () {
            return FlowCoordinator.confirmUnsavedAndHandle(this, function () {
                return Promise.resolve(false);
            }).then(function (sDecision) {
                return SearchSelectionNavigationUseCase.shouldProceedAfterUnsavedDecision(sDecision);
            });
        },

        _syncSearchSelectionState: function () {
            SearchSelectionNavigationUseCase.syncSelectionState({
                selectedModel: this.getModel("selected"),
                dataModel: this.getModel("data"),
                viewModel: this._getViewModel()
            });

            SearchToolbarActionStateUseCase.applyActionStateToViewModel({
                selectedModel: this.getModel("selected"),
                dataModel: this.getModel("data"),
                viewModel: this._getViewModel(),
                isLoading: this.getModel("state").getProperty("/isLoading"),
                useSmartControls: this._isSmartControlsEnabled()
            });
        },

        onCreate: function () {
            return SearchCreateCopyNavigationGuardUseCase.runCreateNavigationFlow({
                confirmNavigation: this._confirmNavigationFromDirty.bind(this),
                buildCreateIntent: SearchNavigationIntentUseCase.buildCreateIntent,
                applyIntent: function (mIntent) {
                    return SearchNavigationIntentUseCase.applyIntent({
                        intent: mIntent,
                        stateModel: this.getModel("state"),
                        navTo: this.navTo.bind(this)
                    });
                }.bind(this)
            });
        },

        onCopy: function () {
            return SearchCreateCopyNavigationGuardUseCase.runCopyNavigationFlow({
                resolveSelectedId: function () {
                    return SearchNavigationIntentUseCase.resolveSelectedId({
                        selectedModel: this.getModel("selected")
                    });
                }.bind(this),
                confirmNavigation: this._confirmNavigationFromDirty.bind(this),
                buildCopyIntent: function (sSelectedId) {
                    return SearchNavigationIntentUseCase.buildCopyIntent({ selectedId: sSelectedId });
                },
                applyIntent: function (mIntent) {
                    return SearchNavigationIntentUseCase.applyIntent({
                        intent: mIntent,
                        stateModel: this.getModel("state"),
                        navTo: this.navTo.bind(this)
                    });
                }.bind(this)
            }).then(function (oResult) {
                if (oResult && oResult.reason === "missing_selection") {
                    SearchActionMessagePresentationUseCase.presentCopyMissingSelection({
                        bundle: this.getResourceBundle(),
                        showToast: MessageToast.show
                    });
                }
                return oResult;
            }.bind(this));
        },

        onDelete: function () {
            var oBundle = this.getResourceBundle();
            return SearchDeleteOrchestrationUseCase.runDeleteFlow({
                resolveSelectedId: function () {
                    return SearchNavigationIntentUseCase.resolveSelectedId({
                        selectedModel: this.getModel("selected")
                    });
                }.bind(this),
                runWithLoading: function (fnTask) {
                    return this.runWithStateFlag(this.getModel("state"), "/isLoading", fnTask);
                }.bind(this),
                deleteAndReload: SearchApplicationService.deleteChecklistAndReload,
                applyRows: function (aUpdated) {
                    this.getModel("data").setProperty("/checkLists", aUpdated);
                }.bind(this),
                rebind: this._rebindSmartTable.bind(this),
                applySelectedChecklist: function (oChecklist) {
                    SearchSelectionNavigationUseCase.applySelectedChecklist({
                        checklist: oChecklist,
                        selectedModel: this.getModel("selected"),
                        viewModel: this._getViewModel()
                    });
                }.bind(this),
                reloadSelectionState: this._syncSearchSelectionState.bind(this)
            }).then(function (oResult) {
                SearchActionMessagePresentationUseCase.presentDeleteFlowResult({
                    bundle: oBundle,
                    showToast: MessageToast.show,
                    result: oResult
                });
            });
        },

        onSmartSearch: function () {
            this._syncStateFiltersFromSmartFilter();
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
            var mPayload = this._buildAnalyticsPayload();
            var sSearchMode = this.getModel("state").getProperty("/searchMode") || "EXACT";
            var aFallback = this.getModel("data").getProperty("/checkLists") || [];

            return SearchWorkflowAnalyticsDialogUseCase.runAnalyticsLoadFlow({
                viewModel: this._getViewModel(),
                loadAnalytics: function () {
                    return WorkflowAnalyticsUseCase.loadProcessAnalytics(mPayload, sSearchMode, aFallback);
                }
            });
        },

        onOpenWorkflowAnalytics: function () {
            SearchWorkflowAnalyticsDialogUseCase.openDialogLifecycle({
                dialog: this.byId("workflowAnalyticsDialog"),
                runLoad: this._loadWorkflowAnalytics.bind(this),
                openDialog: SearchAnalyticsDialogExportFlowUseCase.openAnalyticsDialog
            });
        },

        onCloseWorkflowAnalytics: function () {
            SearchWorkflowAnalyticsDialogUseCase.closeDialogLifecycle({
                dialog: this.byId("workflowAnalyticsDialog"),
                closeDialog: SearchAnalyticsDialogExportFlowUseCase.closeAnalyticsDialog
            });
        },

        _collectScreenRowsForExport: function () {
            var aRows = this.getModel("data").getProperty("/visibleCheckLists") || [];
            return SearchActionUseCase.buildExportRowsFromVisible(aRows);
        },

        _runExport: function (sEntity) {
            var mPayload = this._buildFilterPayload();
            var sSearchMode = this.getModel("state").getProperty("/searchMode") || "EXACT";
            var oBundle = this.getResourceBundle();

            return SearchExportOrchestrationUseCase.runExportLifecycle({
                runExportFlow: SearchAnalyticsDialogExportFlowUseCase.runExportFlow,
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
                    ExcelExport.download(SearchExportOrchestrationUseCase.buildExportFilename(sEntity), aRows);
                    MessageToast.show(oBundle.getText("exportDone", [aRows.length]));
                },
                onError: function (oError) {
                    MessageToast.show(oBundle.getText("exportFailed", [((oError && oError.message) || "Unknown error")]));
                }
            });
        },

        onExportMenuDefault: function () {
            return SearchExportIntentGuardUseCase.runExportIntent({
                defaultEntity: "screen",
                allowedEntities: ["screen", "barrier", "check"],
                isEnabled: function () {
                    return !!this._getViewModel().getProperty("/canExport");
                }.bind(this),
                runExport: this._runExport.bind(this)
            });
        },

        onExportMenuAction: function (oEvent) {
            return SearchExportIntentGuardUseCase.runExportIntent({
                event: oEvent,
                defaultEntity: "screen",
                allowedEntities: ["screen", "barrier", "check"],
                resolveEntityFromMenuEvent: SearchUiFlowUseCase.resolveExportEntityFromMenuEvent,
                isEnabled: function () {
                    return !!this._getViewModel().getProperty("/canExport");
                }.bind(this),
                runExport: this._runExport.bind(this)
            });
        },

        onExportReport: function (oEvent) {
            return SearchExportIntentGuardUseCase.runExportIntent({
                source: oEvent.getSource(),
                defaultEntity: "screen",
                allowedEntities: ["screen", "barrier", "check"],
                isEnabled: function () {
                    return !!this._getViewModel().getProperty("/canExport");
                }.bind(this),
                runExport: this._runExport.bind(this)
            });
        },

        onRetryLoad: function () {
            var oStateModel = this.getModel("state");
            var oBundle = this.getResourceBundle();

            return SearchRetryLoadPresentationUseCase.runRetryFlow({
                stateModel: oStateModel,
                dataModel: this.getModel("data"),
                runWithLoading: function (fnTask) {
                    return this.runWithStateFlag(oStateModel, "/isLoading", fnTask);
                }.bind(this),
                getCheckLists: SearchApplicationService.getCheckLists,
                onAfterApply: function () {
                    this._syncSearchSelectionState();
                    this._updateResultSummary();
                }.bind(this),
                treatEmptyAsError: false,
                maxAttempts: 2
            }).then(function (oResult) {
                SearchRetryMessagePresentationUseCase.presentRetryOutcome({
                    result: oResult,
                    bundle: oBundle,
                    stateModel: oStateModel,
                    showToast: MessageToast.show,
                    unknownFallbackKey: "loadErrorMessage"
                });
                this._applyEmptyStatePresentation();
                return oResult;
            });
        },

        onResetFilters: function () {
            SearchLoadFilterUseCase.resetFilters(this.getModel("state"), this._getViewModel());
            if (this._isSmartControlsEnabled()) {
                var oSmartFilterBar = this.byId("searchSmartFilterBar");
                if (oSmartFilterBar && oSmartFilterBar.clear) {
                    oSmartFilterBar.clear();
                }
            }
            this._rebindSmartTable();
            this._syncSearchSelectionState();
        },


        onMaxRowsChange: function () {
            var oState = this.getModel("state");
            var sNormalized = SearchPresentationUseCase.normalizeMaxRowsInput(oState.getProperty("/searchMaxResults"));
            oState.setProperty("/searchMaxResults", sNormalized);
        },

        onSearchModeToggle: function (oEvent) {
            var bLoose = oEvent.getParameter("state");
            SearchLoadFilterUseCase.applySearchMode(this.getModel("state"), this._getViewModel(), bLoose);
            this._updateFilterState();
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
