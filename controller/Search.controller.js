sap.ui.define([
    "sap_ui5/controller/Base.controller",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageToast",
    "sap_ui5/service/backend/BackendAdapter",
    "sap_ui5/service/SmartSearchAdapter"
], function (BaseController, JSONModel, MessageToast, BackendAdapter, SmartSearchAdapter) {
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
                smartTableReady: true
            });

            this._iSearchDebounceMs = 180;
            this._iSearchTimer = null;

            this.applyStoredTheme();
            this.setModel(oViewModel, "view");

            oLayoutModel.setProperty("/smartFilter/fields", SmartSearchAdapter.getSmartFilterConfig().fields);
            oLayoutModel.setProperty("/smartTable/columns", SmartSearchAdapter.getSmartTableConfig().columns);

            ["/filterId", "/filterLpc", "/filterFailedChecks", "/filterFailedBarriers"].forEach(function (sPath) {
                oStateModel.bindProperty(sPath).attachChange(this._onFilterChanged, this);
            }.bind(this));

            this.attachRouteMatched("search", this._onSearchMatched);
            this._updateFilterState();
            this._updateResultSummary();
        },

        _onSearchMatched: function () {
            var oStateModel = this.getModel("state");

            oStateModel.setProperty("/layout", "OneColumn");
            oStateModel.setProperty("/mode", "READ");
            this._updateResultSummary();
        },

        _onFilterChanged: function () {
            this._updateFilterState();

            if (this.getView().getModel("view").getProperty("/hasSearched")) {
                this._scheduleSearch();
            }
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

            return {
                filterId: oStateModel.getProperty("/filterId"),
                filterLpc: oStateModel.getProperty("/filterLpc"),
                filterFailedChecks: oStateModel.getProperty("/filterFailedChecks"),
                filterFailedBarriers: oStateModel.getProperty("/filterFailedBarriers")
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

            oStateModel.setProperty("/isLoading", true);

            return BackendAdapter.queryCheckLists({
                idContains: mPayload.filterId,
                lpcKey: mPayload.filterLpc
            }).then(function (aPrefiltered) {
                var aFiltered = SmartSearchAdapter.filterData(aPrefiltered, mPayload, sSearchMode);
                oDataModel.setProperty("/visibleCheckLists", aFiltered);
                this._updateResultSummary();
            }.bind(this)).catch(function () {
                var aSource = oDataModel.getProperty("/checkLists") || [];
                var aFiltered = SmartSearchAdapter.filterData(aSource, mPayload, sSearchMode);
                oDataModel.setProperty("/visibleCheckLists", aFiltered);
                this._updateResultSummary();
            }.bind(this)).finally(function () {
                oStateModel.setProperty("/isLoading", false);
            });
        },

        onSelect: function (oEvent) {
            this.getView().getModel("view").setProperty("/hasSelection", true);
            var oCtx = oEvent.getParameter("listItem").getBindingContext("data");
            var oChecklist = oCtx.getObject();
            var sId = oChecklist && oChecklist.root ? oChecklist.root.id : "";

            if (!sId) {
                MessageToast.show(this.getResourceBundle().getText("checklistIdMissing"));
                return;
            }

            this.getModel("selected").setData(oChecklist);
            this.getModel("state").setProperty("/layout", "TwoColumnsMidExpanded");
            this.navTo("detail", { id: sId });
        },

        onCreate: function () {
            this.getModel("state").setProperty("/objectAction", "CREATE");
            this.getModel("state").setProperty("/layout", "TwoColumnsMidExpanded");
            this.navTo("object", { id: "__create" });
        },

        onCopy: function () {
            var oSelected = this.getModel("selected").getData() || {};
            var sId = oSelected && oSelected.root ? oSelected.root.id : "";

            if (!sId) {
                MessageToast.show(this.getResourceBundle().getText("nothingToCopy"));
                return;
            }

            this.getModel("state").setProperty("/objectAction", "COPY");
            this.getModel("state").setProperty("/layout", "TwoColumnsMidExpanded");
            this.navTo("object", { id: sId });
        },

        onDelete: function () {
            var oSelected = this.getModel("selected").getData() || {};
            var sId = oSelected && oSelected.root ? oSelected.root.id : "";
            var oBundle = this.getResourceBundle();

            if (!sId) {
                MessageToast.show(oBundle.getText("nothingToDelete"));
                return;
            }

            this.getModel("state").setProperty("/isBusy", true);

            BackendAdapter.deleteCheckList(sId).then(function () {
                return BackendAdapter.getCheckLists();
            }).then(function (aUpdated) {
                this.getModel("data").setProperty("/checkLists", aUpdated);
                this._executeSearch();
                this.getModel("selected").setData({});
                this.getView().getModel("view").setProperty("/hasSelection", false);
                MessageToast.show(oBundle.getText("deleted"));
            }.bind(this)).catch(function (oError) {
                MessageToast.show(oBundle.getText("deleteFailed", [((oError && oError.message) || "Unknown error")]));
            }).finally(function () {
                this.getModel("state").setProperty("/isBusy", false);
            }.bind(this));
        },

        onSearch: function () {
            this.getView().getModel("view").setProperty("/hasSearched", true);
            this._executeSearch();
        },


        onRetryLoad: function () {
            var oStateModel = this.getModel("state");
            var oDataModel = this.getModel("data");

            oStateModel.setProperty("/isLoading", true);
            oStateModel.setProperty("/loadError", false);
            oStateModel.setProperty("/loadErrorMessage", "");

            return BackendAdapter.getCheckLists().then(function (aCheckLists) {
                oDataModel.setProperty("/checkLists", aCheckLists);
                oDataModel.setProperty("/visibleCheckLists", aCheckLists);
                this._updateResultSummary();
            }.bind(this)).catch(function (oError) {
                oStateModel.setProperty("/loadError", true);
                oStateModel.setProperty("/loadErrorMessage", (oError && oError.message) || "");
            }).finally(function () {
                oStateModel.setProperty("/isLoading", false);
            });
        },

        onResetFilters: function () {
            var oStateModel = this.getModel("state");

            oStateModel.setProperty("/filterId", "");
            oStateModel.setProperty("/filterLpc", "");
            oStateModel.setProperty("/filterFailedChecks", "ALL");
            oStateModel.setProperty("/filterFailedBarriers", "ALL");

            this.getView().getModel("view").setProperty("/hasSearched", true);
            this._executeSearch();
        },

        onSearchModeToggle: function (oEvent) {
            var bLoose = oEvent.getParameter("state");
            this.getModel("state").setProperty("/searchMode", bLoose ? "LOOSE" : "EXACT");
            this.getView().getModel("view").setProperty("/hasSearched", true);
            this._executeSearch();
        },

        onExit: function () {
            if (this._iSearchTimer) {
                clearTimeout(this._iSearchTimer);
                this._iSearchTimer = null;
            }
        },

        formatStatus: function (sStatus) {
            switch (sStatus) {
                case "SUCCESS": return "Success";
                case "WARNING": return "Warning";
                case "CRITICAL": return "Error";
                default: return "None";
            }
        }
    });
});
