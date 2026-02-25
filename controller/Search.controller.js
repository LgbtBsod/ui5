sap.ui.define([
    "sap_ui5/controller/Base.controller",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageToast",
    "sap/m/MessageBox",
    "sap_ui5/service/backend/BackendAdapter",
    "sap_ui5/service/SmartSearchAdapter",
    "sap_ui5/util/ExcelExport",
    "sap_ui5/util/FlowCoordinator",
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator"
], function (BaseController, JSONModel, MessageToast, MessageBox, BackendAdapter, SmartSearchAdapter, ExcelExport, FlowCoordinator, Filter, FilterOperator) {
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
                smartFilterReady: true,
                smartTableReady: true,
                useSmartControls: true,
                smartControlError: ""
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

            this.attachRouteMatched("search", this._onSearchMatched);
            this._bootstrapSmartControls();
            this._updateFilterState();
            this._updateResultSummary();
        },



        _isSmartControlsEnabled: function () {
            return !!this.getView().getModel("view").getProperty("/useSmartControls");
        },

        _setSmartControlsEnabled: function (bEnabled, sError) {
            var oViewModel = this.getView().getModel("view");
            oViewModel.setProperty("/useSmartControls", !!bEnabled);
            oViewModel.setProperty("/smartControlError", sError || "");
        },

        _bootstrapSmartControls: async function () {
            var oSmartTable = this.byId("searchSmartTable");
            if (!oSmartTable) {
                this._setSmartControlsEnabled(false, "SmartTable control not found");
                return;
            }

            try {
                if (oSmartTable.isInitialised && oSmartTable.isInitialised()) {
                    this._onSmartTableReady();
                    return;
                }

                await new Promise((resolve) => {
                    oSmartTable.attachInitialise(resolve);
                });
                this._onSmartTableReady();
            } catch (oError) {
                this._setSmartControlsEnabled(false, (oError && oError.message) || "Smart controls init failed");
            }
        },

        _onSmartTableReady: function () {
            var oSmartTable = this.byId("searchSmartTable");
            if (!oSmartTable || !oSmartTable.getTable) {
                this._setSmartControlsEnabled(false, "SmartTable inner table unavailable");
                return;
            }

            this._oSmartInnerTable = oSmartTable.getTable();
            if (!this._oSmartInnerTable) {
                this._setSmartControlsEnabled(false, "SmartTable inner table missing");
                return;
            }

            if (this._oSmartInnerTable.attachItemPress) {
                this._oSmartInnerTable.attachItemPress(this._onSmartTableSelectionChange, this);
            }
            if (this._oSmartInnerTable.attachSelectionChange) {
                this._oSmartInnerTable.attachSelectionChange(this._onSmartTableSelectionChange, this);
            }
        },

        _extractChecklistIdFromObject: function (oObject) {
            if (!oObject) {
                return "";
            }
            return String(
                oObject.id
                || oObject.ID
                || oObject.ChecklistId
                || oObject.CHECKLIST_ID
                || (((oObject.root || {}).id) || "")
            ).trim();
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

            var oChecklist = await BackendAdapter.getChecklistRoot(sId).catch(function () { return null; });
            if (!oChecklist || !oChecklist.root) {
                oChecklist = { root: { id: sId } };
            }

            this.getModel("selected").setData(oChecklist);
            this.getView().getModel("view").setProperty("/hasSelection", true);
            this.getModel("state").setProperty("/layout", "TwoColumnsMidExpanded");
            this.navTo("detail", { id: sId });
        },

        _onSmartTableSelectionChange: function (oEvent) {
            var oListItem = oEvent.getParameter("listItem") || oEvent.getParameter("item");
            var oCtx = oListItem ? oListItem.getBindingContext() : null;
            var oObj = oCtx ? oCtx.getObject() : null;
            var sId = this._extractChecklistIdFromObject(oObj);
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
            var oBindingParams = oEvent.getParameter("bindingParams") || {};
            var aFilters = oBindingParams.filters || [];

            var sFilterId = String(oState.getProperty("/filterId") || "").trim();
            var sFilterLpc = oState.getProperty("/filterLpc") || "";

            if (sFilterId) {
                aFilters.push(new Filter({
                    filters: [
                        new Filter("id", FilterOperator.Contains, sFilterId),
                        new Filter("checklist_id", FilterOperator.Contains, sFilterId)
                    ],
                    and: false
                }));
            }
            if (sFilterLpc) {
                aFilters.push(new Filter("LPC_KEY", FilterOperator.EQ, sFilterLpc));
            }

            var sChecks = oState.getProperty("/filterFailedChecks") || "ALL";
            var sBarriers = oState.getProperty("/filterFailedBarriers") || "ALL";
            if (sChecks !== "ALL") {
                aFilters.push(new Filter("has_failed_checks", FilterOperator.EQ, sChecks === "TRUE"));
            }
            if (sBarriers !== "ALL") {
                aFilters.push(new Filter("has_failed_barriers", FilterOperator.EQ, sBarriers === "TRUE"));
            }

            var sMax = String(oState.getProperty("/searchMaxResults") || "").trim();
            var iMax = sMax ? Math.max(1, Math.min(9999, Number(sMax) || 0)) : 0;
            oBindingParams.parameters = oBindingParams.parameters || {};
            if (iMax > 0) {
                oBindingParams.parameters.top = iMax;
            }

            oBindingParams.filters = aFilters;
        },

        _rebindSmartTable: function () {
            var oSmartTable = this.byId("searchSmartTable");
            if (oSmartTable && oSmartTable.rebindTable) {
                oSmartTable.rebindTable();
            }
        },

        _onSearchMatched: function () {
            var oStateModel = this.getModel("state");

            oStateModel.setProperty("/layout", "OneColumn");
            oStateModel.setProperty("/mode", "READ");
            this._updateResultSummary();
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

            this.getView().getModel("view").setProperty("/hasActiveFilters", bHasFilters);
        },

        _updateResultSummary: function () {
            var oDataModel = this.getModel("data");
            var oBundle = this.getResourceBundle();
            var iVisible = (oDataModel.getProperty("/visibleCheckLists") || []).length;
            var iTotal = (oDataModel.getProperty("/checkLists") || []).length;

            this.getView().getModel("view").setProperty("/resultSummary", oBundle.getText("resultSummary", [iVisible, iTotal]));
        },

        _executeSearch: function () {
            var oDataModel = this.getModel("data");
            var oStateModel = this.getModel("state");
            var mPayload = this._buildFilterPayload();
            var sSearchMode = oStateModel.getProperty("/searchMode") || "EXACT";

            return this.runWithStateFlag(oStateModel, "/isLoading", function () {
                return BackendAdapter.queryCheckLists({
                    idContains: mPayload.filterId,
                    lpcKey: mPayload.filterLpc,
                    maxResults: mPayload.maxResults
                }).then(function (aPrefiltered) {
                    var aFiltered = SmartSearchAdapter.filterData(aPrefiltered, mPayload, sSearchMode);
                    oDataModel.setProperty("/visibleCheckLists", aFiltered);
                    this._updateResultSummary();
                }.bind(this)).catch(function () {
                    var aSource = oDataModel.getProperty("/checkLists") || [];
                    var aFiltered = SmartSearchAdapter.filterData(aSource, mPayload, sSearchMode);
                    oDataModel.setProperty("/visibleCheckLists", aFiltered);
                    this._updateResultSummary();
                }.bind(this));
            }.bind(this));
        },

        onSelect: async function (oEvent) {
            this.getView().getModel("view").setProperty("/hasSelection", true);

            var oListItem = oEvent.getParameter("listItem");
            var oSource = oEvent.getSource();

            if (!oListItem && oSource && typeof oSource.getSelectedItem === "function") {
                oListItem = oSource.getSelectedItem();
            }

            var oCtx = oListItem ? oListItem.getBindingContext("data") : null;
            var oChecklist = oCtx ? oCtx.getObject() : null;
            var sId = oChecklist && oChecklist.root ? oChecklist.root.id : "";
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
            var sId = oSelected && oSelected.root ? oSelected.root.id : "";

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
            var sId = oSelected && oSelected.root ? oSelected.root.id : "";
            var oBundle = this.getResourceBundle();

            if (!sId) {
                MessageToast.show(oBundle.getText("nothingToDelete"));
                return;
            }

            this.runWithStateFlag(this.getModel("state"), "/isLoading", function () {
                return BackendAdapter.deleteCheckList(sId).then(function () {
                    return BackendAdapter.getCheckLists();
                }).then(function (aUpdated) {
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
            this.getView().getModel("view").setProperty("/hasSearched", true);
            this._rebindSmartTable();
        },

        onSearch: function () {
            this.getView().getModel("view").setProperty("/hasSearched", true);
            this._rebindSmartTable();
        },

        onStatusFilterPress: function (oEvent) {
            var oSource = oEvent.getSource();
            var sFilterPath = oSource.data("filterPath");
            var sFilterValue = oSource.data("filterValue");

            if (!sFilterPath) {
                return;
            }

            this.getModel("state").setProperty(sFilterPath, sFilterValue);
            this._rebindSmartTable();
        },



        onExportReport: function (oEvent) {
            var sEntity = oEvent.getSource().data("entity") || "checklist";
            var mPayload = this._buildFilterPayload();
            var sSearchMode = this.getModel("state").getProperty("/searchMode") || "EXACT";
            var oBundle = this.getResourceBundle();

            this.runWithStateFlag(this.getModel("state"), "/isLoading", function () {
                return BackendAdapter.exportReport(sEntity, {
                    filters: mPayload,
                    searchMode: sSearchMode
                }).then(function (oResult) {
                    var aRows = (oResult && oResult.rows) || [];
                    if (!aRows.length) {
                        MessageToast.show(oBundle.getText("exportEmpty"));
                        return;
                    }
                    ExcelExport.download("checklist_" + sEntity + "_" + Date.now(), aRows);
                    MessageToast.show(oBundle.getText("exportDone", [aRows.length]));
                }).catch(function (oError) {
                    MessageToast.show(oBundle.getText("exportFailed", [((oError && oError.message) || "Unknown error")]));
                });
            }.bind(this));
        },

        onRetryLoad: function () {
            var oStateModel = this.getModel("state");
            var oDataModel = this.getModel("data");

            oStateModel.setProperty("/loadError", false);
            oStateModel.setProperty("/loadErrorMessage", "");

            return this.runWithStateFlag(oStateModel, "/isLoading", function () {
                return BackendAdapter.getCheckLists().then(function (aCheckLists) {
                    oDataModel.setProperty("/checkLists", aCheckLists);
                    oDataModel.setProperty("/visibleCheckLists", aCheckLists);
                    this._updateResultSummary();
                }.bind(this)).catch(function (oError) {
                    oStateModel.setProperty("/loadError", true);
                    oStateModel.setProperty("/loadErrorMessage", (oError && oError.message) || "");
                });
            }.bind(this));
        },

        onResetFilters: function () {
            var oStateModel = this.getModel("state");

            oStateModel.setProperty("/filterId", "");
            oStateModel.setProperty("/filterLpc", "");
            oStateModel.setProperty("/filterFailedChecks", "ALL");
            oStateModel.setProperty("/filterFailedBarriers", "ALL");

            this.getView().getModel("view").setProperty("/hasSearched", true);
            this._rebindSmartTable();
        },

        onSearchModeToggle: function (oEvent) {
            var bLoose = oEvent.getParameter("state");
            this.getModel("state").setProperty("/searchMode", bLoose ? "LOOSE" : "EXACT");
            this.getView().getModel("view").setProperty("/hasSearched", true);
            this._rebindSmartTable();
        },

        onExit: function () {
            if (this._iSearchTimer) {
                clearTimeout(this._iSearchTimer);
                this._iSearchTimer = null;
            }
        },


        formatOverallResultText: function (vResult, sLifecycleStatus) {
            if (vResult === true) { return this.getResourceBundle().getText("statusOk"); }
            if (vResult === false) { return this.getResourceBundle().getText("statusFailed"); }
            return sLifecycleStatus || "-";
        },

        formatOverallResultState: function (vResult, sLifecycleStatus) {
            if (vResult === true) { return "Success"; }
            if (vResult === false) { return "Error"; }
            switch (String(sLifecycleStatus || "").toUpperCase()) {
                case "CLOSED": return "Success";
                case "REGISTERED": return "Warning";
                default: return "Information";
            }
        },

        formatStatus: function (sStatus) {
            switch (String(sStatus || "").toUpperCase()) {
                case "SUCCESS":
                case "CLOSED": return "Success";
                case "WARNING":
                case "REGISTERED": return "Warning";
                case "CRITICAL": return "Error";
                default: return "None";
            }
        }
    });
});
