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
    "sap_ui5/service/usecase/SearchNavigationIntentUseCase",
    "sap_ui5/service/usecase/SearchStateSyncUseCase",
    "sap_ui5/service/usecase/SearchExecuteFlowUseCase",
    "sap_ui5/service/usecase/SearchCreateCopyFlowUseCase",
    "sap_ui5/service/usecase/SearchDeleteOrchestrationUseCase",
    "sap_ui5/service/usecase/SearchActionMessagePresentationUseCase",
    "sap_ui5/service/usecase/SearchExportIntentGuardUseCase",
    "sap_ui5/service/usecase/SearchRetryMessagePresentationUseCase",
    "sap_ui5/service/usecase/SearchSummaryPresentationUseCase",
    "sap_ui5/service/usecase/SearchEmptyStatePresentationUseCase",
    "sap_ui5/service/usecase/SearchFilterHintPresentationUseCase",
    "sap_ui5/service/usecase/SearchInlineAnalyticsPresentationUseCase",
    "sap_ui5/service/usecase/SearchInlineAnalyticsRefreshOrchestrationUseCase",
    "sap_ui5/service/usecase/SearchFilterLifecycleUseCase",
    "sap_ui5/service/usecase/SearchRetryLifecycleUseCase",
    "sap_ui5/service/usecase/SearchLifecycleSyncUseCase",
    "sap_ui5/service/usecase/SearchToolbarLifecycleUseCase",
    "sap_ui5/service/usecase/SearchWorkflowAnalyticsLifecycleUseCase",
    "sap_ui5/service/usecase/SearchStatusFilterLifecycleUseCase",
    "sap_ui5/service/usecase/SearchTriggerPolicyUseCase",
    "sap_ui5/service/usecase/SearchRouteLifecycleUseCase",
    "sap_ui5/service/usecase/SearchRebindLifecycleUseCase",
    "sap_ui5/service/usecase/SearchResultConvergenceLifecycleUseCase",
    "sap_ui5/service/usecase/SearchSelectionLifecycleUseCase",
    "sap_ui5/service/usecase/SearchExportLifecycleUseCase",
    "sap_ui5/service/usecase/OperationalKpiInstrumentationUseCase",
    "sap_ui5/service/usecase/SearchSelectionOpenFlowUseCase",
    "sap_ui5/util/ExcelExport",
    "sap_ui5/util/FlowCoordinator",
    "sap_ui5/util/SearchWorkflowOrchestrator",
    "sap_ui5/util/SearchSmartControlCoordinator"
], function (BaseController, JSONModel, MessageToast, MessageBox, SmartSearchAdapter, SearchApplicationService, WorkflowAnalyticsUseCase, SearchActionUseCase, SearchUiFlowUseCase, SearchIntentUseCase, SearchLoadFilterUseCase, SearchRetryLoadPresentationUseCase, SearchAnalyticsExportUseCase, SearchAnalyticsDialogExportFlowUseCase, SearchPresentationUseCase, SearchSelectionNavigationUseCase, SearchSmartFilterFlowUseCase, SearchWorkflowAnalyticsDialogUseCase, SearchExportOrchestrationUseCase, SearchNavigationIntentUseCase, SearchStateSyncUseCase, SearchExecuteFlowUseCase, SearchCreateCopyFlowUseCase, SearchDeleteOrchestrationUseCase, SearchActionMessagePresentationUseCase, SearchExportIntentGuardUseCase, SearchRetryMessagePresentationUseCase, SearchSummaryPresentationUseCase, SearchEmptyStatePresentationUseCase, SearchFilterHintPresentationUseCase, SearchInlineAnalyticsPresentationUseCase, SearchInlineAnalyticsRefreshOrchestrationUseCase, SearchFilterLifecycleUseCase, SearchRetryLifecycleUseCase, SearchLifecycleSyncUseCase, SearchToolbarLifecycleUseCase, SearchWorkflowAnalyticsLifecycleUseCase, SearchStatusFilterLifecycleUseCase, SearchTriggerPolicyUseCase, SearchRouteLifecycleUseCase, SearchRebindLifecycleUseCase, SearchResultConvergenceLifecycleUseCase, SearchSelectionLifecycleUseCase, SearchExportLifecycleUseCase, OperationalKpiInstrumentationUseCase, SearchSelectionOpenFlowUseCase, ExcelExport, FlowCoordinator, SearchWorkflowOrchestrator, SearchSmartControlCoordinator) {
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
                smartControlsReasonCode: "",
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
                analyticsRail: {
                    total: 0,
                    failedChecks: 0,
                    failedBarriers: 0,
                    healthy: 0,
                    avgChecksRate: 0,
                    avgBarriersRate: 0,
                    refreshedAt: "-",
                    source: "fallback",
                    sourceText: "fallback"
                },
                filterHintVisible: false,
                filterHintType: "Information",
                filterHintText: "",
                noDataText: "",
                emptyStateKind: "default_empty"
            });

            this._iSearchDebounceMs = 180;
            this._iSearchTimer = null;
            this._inlineAnalyticsRefreshState = SearchInlineAnalyticsRefreshOrchestrationUseCase.ensureRefreshState({});

            this.applyStoredTheme();
            this.setModel(oViewModel, "view");
            SearchInlineAnalyticsPresentationUseCase.applyInlineAnalyticsPresentation({
                viewModel: oViewModel,
                analytics: oViewModel.getProperty("/analytics"),
                bundle: this.getResourceBundle()
            });
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
            return SearchSelectionOpenFlowUseCase.hydrateSelection({
                id: sId,
                selectedModel: this.getModel("selected"),
                viewModel: this._getViewModel(),
                loadChecklistById: SearchApplicationService.getChecklistById,
                syncSelectionState: this._syncSearchSelectionState.bind(this)
            }).then(function (oResult) {
                return (oResult && oResult.checklist) || null;
            });
        },

        _openDetailById: function (sId) {
            return SearchSelectionOpenFlowUseCase.openDetail({
                id: sId,
                confirmNavigation: this._confirmNavigationFromDirty.bind(this),
                stateModel: this.getModel("state"),
                navTo: this.navTo.bind(this),
                bundle: this.getResourceBundle(),
                showToast: MessageToast.show
            });
        },

        _onSmartTableSelectionChange: function (oEvent) {
            return SearchSelectionLifecycleUseCase.runSelectionChangeLifecycle({
                event: oEvent,
                extractId: SearchSmartControlCoordinator.extractChecklistIdFromSelectionEvent,
                hydrateSelection: this._loadSelectedChecklistById.bind(this)
            });
        },

        _onSmartTableItemPress: function (oEvent) {
            return SearchSelectionLifecycleUseCase.runItemPressLifecycle({
                event: oEvent,
                extractId: SearchSmartControlCoordinator.extractChecklistIdFromSelectionEvent,
                hydrateSelection: this._loadSelectedChecklistById.bind(this),
                openDetail: this._openDetailById.bind(this)
            });
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
            SearchTriggerPolicyUseCase.runTriggerPolicy({
                trigger: "SMART_FILTER_CHANGE",
                syncFilterHint: this._updateFilterState.bind(this)
            });
        },

        onBeforeSmartTableRebind: function (oEvent) {
            if (!this._isSmartControlsEnabled()) {
                return;
            }

            SearchRebindLifecycleUseCase.runRebindLifecycle({
                bindingParams: oEvent.getParameter("bindingParams") || {},
                state: {
                    filterId: this.getModel("state").getProperty("/filterId"),
                    filterLpc: this.getModel("state").getProperty("/filterLpc"),
                    filterFailedChecks: this.getModel("state").getProperty("/filterFailedChecks"),
                    filterFailedBarriers: this.getModel("state").getProperty("/filterFailedBarriers"),
                    searchMaxResults: this.getModel("state").getProperty("/searchMaxResults"),
                    searchMode: this.getModel("state").getProperty("/searchMode") || "EXACT"
                },
                smartFilterData: this.byId("searchSmartFilterBar") && this.byId("searchSmartFilterBar").getFilterData ? this.byId("searchSmartFilterBar").getFilterData(true) : {},
                prepareRebind: function (mPrepareArgs) {
                    return SearchSmartFilterFlowUseCase.prepareRebindParams({
                        bindingParams: mPrepareArgs.bindingParams,
                        state: mPrepareArgs.state,
                        onDataReceived: function (oDataEvent) {
                            SearchRebindLifecycleUseCase.runDataReceivedLifecycle({
                                dataEvent: oDataEvent,
                                syncLifecycle: function (oRawData) {
                                    SearchLifecycleSyncUseCase.runSmartTableDataReceivedLifecycle({
                                        rawData: oRawData,
                                        applyMetrics: this._updateSmartTableAnalytics.bind(this),
                                        syncFilterHint: this._updateFilterState.bind(this),
                                        refreshInlineAnalytics: this._refreshInlineAnalyticsByTrigger.bind(this)
                                    });
                                }.bind(this)
                            });
                        }.bind(this),
                        smartFilterData: mPrepareArgs.smartFilterData,
                        applyRebindParams: SearchSmartControlCoordinator.applyRebindParams
                    });
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
            return SearchRouteLifecycleUseCase.runRouteMatchedLifecycle({
                applyDefaults: function () {
                    SearchStateSyncUseCase.applyRouteMatchedDefaults(this.getModel("state"));
                }.bind(this),
                syncSmartControls: this._syncSmartControlAvailability.bind(this),
                syncSelection: this._syncSearchSelectionState.bind(this),
                updateSummary: this._updateResultSummary.bind(this),
                refreshAnalytics: this._refreshInlineAnalyticsByTrigger.bind(this),
                applyEmptyState: this._applyEmptyStatePresentation.bind(this)
            });
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
            SearchFilterHintPresentationUseCase.applyHintPresentation({
                viewModel: this._getViewModel(),
                stateModel: this.getModel("state"),
                useSmartControls: this._isSmartControlsEnabled(),
                hasSmartFilters: this._hasSmartFilters(),
                fallbackPayload: this._buildFilterPayload(),
                bundle: this.getResourceBundle()
            });
        },

        _updateResultSummary: function () {
            return SearchResultConvergenceLifecycleUseCase.runConvergenceLifecycle({
                dataModel: this.getModel("data"),
                applySummary: this._applySearchMetrics.bind(this),
                applyEmptyState: this._applyEmptyStatePresentation.bind(this)
            });
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

            return SearchExecuteFlowUseCase.runExecuteSearchFlow({
                runWithLoading: function (fnTask) {
                    return this.runWithStateFlag(oStateModel, "/isLoading", fnTask);
                }.bind(this),
                runSearch: function () {
                    return SearchApplicationService.runSearch(mPayload, sSearchMode, oDataModel.getProperty("/checkLists") || []);
                },
                applyRows: function (aFiltered) {
                    oDataModel.setProperty("/visibleCheckLists", aFiltered || []);
                },
                afterApply: this._updateResultSummary.bind(this)
            });
        },

        onFallbackSelectionChange: function (oEvent) {
            return SearchSelectionLifecycleUseCase.runSelectionChangeLifecycle({
                event: oEvent,
                extractId: SearchUiFlowUseCase.extractIdFromListSelectionEvent,
                hydrateSelection: this._loadSelectedChecklistById.bind(this)
            });
        },

        onFallbackItemPress: function (oEvent) {
            return SearchSelectionLifecycleUseCase.runItemPressLifecycle({
                event: oEvent,
                extractId: SearchUiFlowUseCase.extractIdFromListSelectionEvent,
                hydrateSelection: this._loadSelectedChecklistById.bind(this),
                openDetail: this._openDetailById.bind(this)
            });
        },



        _confirmNavigationFromDirty: function () {
            return FlowCoordinator.confirmUnsavedAndHandle(this, function () {
                return Promise.resolve(false);
            }).then(function (sDecision) {
                return SearchSelectionNavigationUseCase.shouldProceedAfterUnsavedDecision(sDecision);
            });
        },

        _syncSearchSelectionState: function () {
            return SearchStateSyncUseCase.syncSelectionAndActionState({
                selectedModel: this.getModel("selected"),
                dataModel: this.getModel("data"),
                viewModel: this._getViewModel(),
                isLoading: this.getModel("state").getProperty("/isLoading"),
                useSmartControls: this._isSmartControlsEnabled()
            });
        },

        onCreate: function () {
            return SearchToolbarLifecycleUseCase.runCreateLifecycle({
                runCreateFlow: function () {
                    return SearchCreateCopyFlowUseCase.runCreateFlow({
                        confirmNavigation: this._confirmNavigationFromDirty.bind(this),
                        stateModel: this.getModel("state"),
                        navTo: this.navTo.bind(this)
                    });
                }.bind(this)
            });
        },

        onCopy: function () {
            return SearchToolbarLifecycleUseCase.runCopyLifecycle({
                runCopyFlow: function () {
                    return SearchCreateCopyFlowUseCase.runCopyFlow({
                        selectedModel: this.getModel("selected"),
                        confirmNavigation: this._confirmNavigationFromDirty.bind(this),
                        stateModel: this.getModel("state"),
                        navTo: this.navTo.bind(this),
                        bundle: this.getResourceBundle(),
                        showToast: MessageToast.show
                    });
                }.bind(this)
            });
        },

        onDelete: function () {
            var oBundle = this.getResourceBundle();
            return SearchToolbarLifecycleUseCase.runDeleteLifecycle({
                runDeleteFlow: function () {
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
                    });
                }.bind(this),
                presentDeleteOutcome: function (oResult) {
                    SearchActionMessagePresentationUseCase.presentDeleteFlowResult({
                        bundle: oBundle,
                        showToast: MessageToast.show,
                        result: oResult
                    });
                }
            }).then(function (oLifecycleResult) {
                return (oLifecycleResult && oLifecycleResult.result) || oLifecycleResult;
            });
        },

        onSmartSearch: function () {
            this._syncStateFiltersFromSmartFilter();
            SearchIntentUseCase.markSearchedAndRebind(this._getViewModel(), this._rebindSmartTable.bind(this));
            SearchTriggerPolicyUseCase.runTriggerPolicy({
                trigger: "SMART_SEARCH",
                syncFilterHint: this._updateFilterState.bind(this),
                refreshInlineAnalytics: this._refreshInlineAnalyticsByTrigger.bind(this)
            });
        },

        onSearch: function () {
            SearchLifecycleSyncUseCase.runFallbackSearchLifecycle({
                markSearchedAndRebind: function () {
                    SearchIntentUseCase.markSearchedAndRebind(this._getViewModel(), this._rebindSmartTable.bind(this));
                }.bind(this),
                syncFilterHint: this._updateFilterState.bind(this),
                refreshInlineAnalytics: this._refreshInlineAnalyticsByTrigger.bind(this)
            });
        },

        onStatusFilterPress: function (oEvent) {
            return SearchStatusFilterLifecycleUseCase.runStatusFilterLifecycle({
                event: oEvent,
                getSource: function (oRawEvent) {
                    return oRawEvent && oRawEvent.getSource ? oRawEvent.getSource() : null;
                },
                readData: function (oSource, sKey) {
                    return oSource && oSource.data ? oSource.data(sKey) : "";
                },
                applyStatusFilter: function (sFilterPath, sFilterValue) {
                    SearchIntentUseCase.applyStatusFilter(this.getModel("state"), sFilterPath, sFilterValue, this._rebindSmartTable.bind(this));
                }.bind(this),
                afterApply: function () {
                    SearchTriggerPolicyUseCase.runTriggerPolicy({
                        trigger: "STATUS_FILTER_PRESS",
                        syncFilterHint: this._updateFilterState.bind(this),
                        refreshInlineAnalytics: this._refreshInlineAnalyticsByTrigger.bind(this)
                    });
                }.bind(this)
            });
        },





        _buildAnalyticsPayload: function () {
            var mPayload = this._buildFilterPayload();
            return SearchActionUseCase.buildAnalyticsPayload(mPayload);
        },

        _loadWorkflowAnalytics: function () {
            var mPayload = this._buildAnalyticsPayload();
            var sSearchMode = this.getModel("state").getProperty("/searchMode") || "EXACT";
            var aFallback = this.getModel("data").getProperty("/checkLists") || [];

            return SearchWorkflowAnalyticsLifecycleUseCase.runLoadLifecycle({
                runLoadFlow: function () {
                    return SearchWorkflowAnalyticsDialogUseCase.runAnalyticsLoadFlow({
                        viewModel: this._getViewModel(),
                        loadAnalytics: function () {
                            return WorkflowAnalyticsUseCase.loadProcessAnalytics(mPayload, sSearchMode, aFallback);
                        }
                    });
                }.bind(this),
                applyLoadError: function (sErrorText) {
                    this._getViewModel().setProperty("/analyticsError", sErrorText || "");
                }.bind(this)
            }).then(function (oLifecycleResult) {
                return (oLifecycleResult && oLifecycleResult.result) || null;
            });
        },

        _refreshInlineAnalyticsByTrigger: function (sTrigger) {
            if (!SearchInlineAnalyticsRefreshOrchestrationUseCase.shouldRefreshForTrigger(sTrigger)) {
                return Promise.resolve({ applied: false, reason: "unsupported_trigger" });
            }

            return SearchInlineAnalyticsRefreshOrchestrationUseCase.runRefreshLifecycle({
                refreshState: this._inlineAnalyticsRefreshState,
                viewModel: this._getViewModel(),
                loadAnalytics: this._loadWorkflowAnalytics.bind(this),
                applyPresentation: function (oAnalytics) {
                    SearchInlineAnalyticsPresentationUseCase.applyInlineAnalyticsPresentation({
                        viewModel: this._getViewModel(),
                        analytics: oAnalytics || this._getViewModel().getProperty("/analytics"),
                        bundle: this.getResourceBundle()
                    });
                }.bind(this)
            });
        },

        onOpenWorkflowAnalytics: function () {
            return SearchWorkflowAnalyticsLifecycleUseCase.runOpenLifecycle({
                applyDegradedState: function () {
                    if (!this._isSmartControlsEnabled()) {
                        this._getViewModel().setProperty("/analyticsError", this.getResourceBundle().getText("smartControlsUnavailable"));
                    }
                }.bind(this),
                openDialog: function () {
                    SearchWorkflowAnalyticsDialogUseCase.openDialogLifecycle({
                        dialog: this.byId("workflowAnalyticsDialog"),
                        runLoad: this._loadWorkflowAnalytics.bind(this),
                        openDialog: SearchAnalyticsDialogExportFlowUseCase.openAnalyticsDialog
                    });
                }.bind(this)
            });
        },

        onCloseWorkflowAnalytics: function () {
            return SearchWorkflowAnalyticsLifecycleUseCase.runCloseLifecycle({
                closeDialog: function () {
                    SearchWorkflowAnalyticsDialogUseCase.closeDialogLifecycle({
                        dialog: this.byId("workflowAnalyticsDialog"),
                        closeDialog: SearchAnalyticsDialogExportFlowUseCase.closeAnalyticsDialog
                    });
                }.bind(this)
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

            return SearchExportLifecycleUseCase.runExportExecutionLifecycle({
                runLifecycle: function () {
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
                }.bind(this)
            }).then(function (oLifecycleResult) {
                return (oLifecycleResult && oLifecycleResult.result) || oLifecycleResult;
            });
        },

        _presentExportIntentResult: function (oResult) {
            SearchExportLifecycleUseCase.runIntentPresentationLifecycle({
                result: oResult,
                present: function (oRawResult) {
                    return SearchActionMessagePresentationUseCase.presentExportIntentResult({
                        result: oRawResult,
                        bundle: this.getResourceBundle(),
                        showToast: MessageToast.show
                    });
                }.bind(this),
                onUnexpected: function () {
                    OperationalKpiInstrumentationUseCase.markRetryFailure(this.getModel("state"));
                    this._setLoadError("Unexpected export state");
                }.bind(this)
            });
            return oResult;
        },

        onExportMenuDefault: function () {
            return SearchToolbarLifecycleUseCase.runExportIntentLifecycle({
                runIntent: function () {
                    return SearchExportIntentGuardUseCase.runExportIntent({
                        defaultEntity: "screen",
                        allowedEntities: ["screen", "barrier", "check"],
                        isEnabled: function () {
                            return !!this._getViewModel().getProperty("/canExport");
                        }.bind(this),
                        runExport: this._runExport.bind(this)
                    });
                }.bind(this),
                presentIntentResult: this._presentExportIntentResult.bind(this)
            }).then(function (oLifecycleResult) {
                return (oLifecycleResult && oLifecycleResult.result) || oLifecycleResult;
            });
        },

        onExportMenuAction: function (oEvent) {
            return SearchToolbarLifecycleUseCase.runExportIntentLifecycle({
                runIntent: function () {
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
                }.bind(this),
                presentIntentResult: this._presentExportIntentResult.bind(this)
            }).then(function (oLifecycleResult) {
                return (oLifecycleResult && oLifecycleResult.result) || oLifecycleResult;
            });
        },

        onExportReport: function (oEvent) {
            return SearchToolbarLifecycleUseCase.runExportIntentLifecycle({
                runIntent: function () {
                    return SearchExportIntentGuardUseCase.runExportIntent({
                        source: oEvent.getSource(),
                        defaultEntity: "screen",
                        allowedEntities: ["screen", "barrier", "check"],
                        isEnabled: function () {
                            return !!this._getViewModel().getProperty("/canExport");
                        }.bind(this),
                        runExport: this._runExport.bind(this)
                    });
                }.bind(this),
                presentIntentResult: this._presentExportIntentResult.bind(this)
            }).then(function (oLifecycleResult) {
                return (oLifecycleResult && oLifecycleResult.result) || oLifecycleResult;
            });
        },

        onRetryLoad: function () {
            var oStateModel = this.getModel("state");
            var oBundle = this.getResourceBundle();

            return SearchRetryLifecycleUseCase.runRetryLifecycle({
                beginLatency: OperationalKpiInstrumentationUseCase.beginLatencySample,
                runRetryFlow: function () {
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
                            this._refreshInlineAnalyticsByTrigger("RETRY_LOAD");
                        }.bind(this),
                        treatEmptyAsError: false,
                        maxAttempts: 2
                    });
                }.bind(this),
                presentRetryOutcome: function (oResult) {
                    return SearchRetryMessagePresentationUseCase.presentRetryOutcome({
                        result: oResult,
                        bundle: oBundle,
                        stateModel: oStateModel,
                        showToast: MessageToast.show,
                        unknownFallbackKey: "loadErrorMessage"
                    });
                },
                markRetryFailure: function () {
                    OperationalKpiInstrumentationUseCase.markRetryFailure(oStateModel);
                },
                finishLatency: function (sMetric, iStartedAt) {
                    OperationalKpiInstrumentationUseCase.finishLatencySample(oStateModel, sMetric, iStartedAt);
                },
                afterRetryApplied: this._applyEmptyStatePresentation.bind(this)
            }).then(function (oLifecycleResult) {
                return (oLifecycleResult && oLifecycleResult.result) || oLifecycleResult;
            });
        },

        onResetFilters: function () {
            SearchFilterLifecycleUseCase.runResetLifecycle({
                resetFilters: function () {
                    SearchLoadFilterUseCase.resetFilters(this.getModel("state"), this._getViewModel());
                }.bind(this),
                clearSmartFilters: function () {
                    if (!this._isSmartControlsEnabled()) {
                        return;
                    }

                    var oSmartFilterBar = this.byId("searchSmartFilterBar");
                    if (oSmartFilterBar && oSmartFilterBar.clear) {
                        oSmartFilterBar.clear();
                    }
                }.bind(this),
                rebind: this._rebindSmartTable.bind(this),
                syncSelectionState: this._syncSearchSelectionState.bind(this),
                refreshInlineAnalytics: function (sTrigger) {
                    SearchTriggerPolicyUseCase.runTriggerPolicy({
                        trigger: sTrigger,
                        syncFilterHint: this._updateFilterState.bind(this),
                        refreshInlineAnalytics: this._refreshInlineAnalyticsByTrigger.bind(this)
                    });
                }.bind(this)
            });
        },


        onMaxRowsChange: function () {
            var oState = this.getModel("state");
            var sNormalized = SearchPresentationUseCase.normalizeMaxRowsInput(oState.getProperty("/searchMaxResults"));
            oState.setProperty("/searchMaxResults", sNormalized);
        },

        onSearchModeToggle: function (oEvent) {
            SearchFilterLifecycleUseCase.runSearchModeToggleLifecycle({
                looseMode: oEvent.getParameter("state"),
                applySearchMode: function (bLoose) {
                    SearchLoadFilterUseCase.applySearchMode(this.getModel("state"), this._getViewModel(), bLoose);
                }.bind(this),
                updateFilterState: this._updateFilterState.bind(this),
                refreshInlineAnalytics: function (sTrigger) {
                    SearchTriggerPolicyUseCase.runTriggerPolicy({
                        trigger: sTrigger,
                        stateModel: this.getModel("state"),
                        syncFilterHint: this._updateFilterState.bind(this),
                        refreshInlineAnalytics: this._refreshInlineAnalyticsByTrigger.bind(this)
                    });
                }.bind(this)
            });
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
