sap.ui.define([
    "sap_ui5/controller/Base.controller",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageToast",
    "sap_ui5/service/backend/BackendAdapter"
], function (BaseController, JSONModel, MessageToast, BackendAdapter) {
    "use strict";

    return BaseController.extend("sap_ui5.controller.Search", {

        onInit: function () {
            var oStateModel = this.getModel("state");
            var oViewModel = new JSONModel({
                hasActiveFilters: false,
                hasSelection: false,
                hasSearched: false,
                resultSummary: ""
            });

            this._iSearchDebounceMs = 180;
            this._iSearchTimer = null;

            this.applyStoredTheme();
            this.setModel(oViewModel, "view");

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

        _updateFilterState: function () {
            var oStateModel = this.getModel("state");
            var bHasFilters = Boolean((oStateModel.getProperty("/filterId") || "").trim())
                || Boolean(oStateModel.getProperty("/filterLpc"))
                || oStateModel.getProperty("/filterFailedChecks") !== "ALL"
                || oStateModel.getProperty("/filterFailedBarriers") !== "ALL";

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
            var aSource = oDataModel.getProperty("/checkLists") || [];

            var sFilterId = (oStateModel.getProperty("/filterId") || "").trim().toLowerCase();
            var sFilterLpc = oStateModel.getProperty("/filterLpc") || "";
            var sFilterFailedChecks = oStateModel.getProperty("/filterFailedChecks") || "ALL";
            var sFilterFailedBarriers = oStateModel.getProperty("/filterFailedBarriers") || "ALL";
            var sSearchMode = oStateModel.getProperty("/searchMode") || "EXACT";
            var aFiltered = aSource.filter(function (oItem) {
                var sId = (((oItem || {}).root || {}).id || "").toLowerCase();
                var sLpc = (((oItem || {}).basic || {}).LPC_KEY || "");
                var nChecks = Number((((oItem || {}).root || {}).successRateChecks));
                var nBarriers = Number((((oItem || {}).root || {}).successRateBarriers));

                var bIdMatch = !sFilterId || sId.includes(sFilterId);
                var bLpcMatch = !sFilterLpc || sLpc === sFilterLpc;
                var bChecksFailed = Number.isFinite(nChecks) && nChecks < 100;
                var bBarriersFailed = Number.isFinite(nBarriers) && nBarriers < 100;

                var bChecksMatch = sFilterFailedChecks === "ALL"
                    || (sFilterFailedChecks === "TRUE" && bChecksFailed)
                    || (sFilterFailedChecks === "FALSE" && !bChecksFailed);

                var bBarriersMatch = sFilterFailedBarriers === "ALL"
                    || (sFilterFailedBarriers === "TRUE" && bBarriersFailed)
                    || (sFilterFailedBarriers === "FALSE" && !bBarriersFailed);

                var aEnabledRules = [
                    { enabled: !!sFilterId, value: bIdMatch },
                    { enabled: !!sFilterLpc, value: bLpcMatch },
                    { enabled: sFilterFailedChecks !== "ALL", value: bChecksMatch },
                    { enabled: sFilterFailedBarriers !== "ALL", value: bBarriersMatch }
                ];

                if (sSearchMode === "LOOSE") {
                    var aActive = aEnabledRules.filter(function (oRule) { return oRule.enabled; });
                    return !aActive.length || aActive.some(function (oRule) { return oRule.value; });
                }

                return bIdMatch && bLpcMatch && bChecksMatch && bBarriersMatch;
            });

            oDataModel.setProperty("/visibleCheckLists", aFiltered);
            this._updateResultSummary();
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
