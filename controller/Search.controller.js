sap.ui.define([
    "sap_ui5/controller/Base.controller",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageToast"
], function (BaseController, JSONModel, MessageToast) {
    "use strict";

    return BaseController.extend("sap_ui5.controller.Search", {

        onInit: function () {
            var oStateModel = this.getModel("state");
            var oViewModel = new JSONModel({
                hasActiveFilters: false
            });

            this.setModel(oViewModel, "view");

            ["/filterId", "/filterLpc", "/filterFailedChecks", "/filterFailedBarriers"].forEach(function (sPath) {
                oStateModel.bindProperty(sPath).attachChange(this._updateFilterState, this);
            }.bind(this));

            this._updateFilterState();
        },

        _updateFilterState: function () {
            var oStateModel = this.getModel("state");
            var bHasFilters = Boolean((oStateModel.getProperty("/filterId") || "").trim())
                || Boolean(oStateModel.getProperty("/filterLpc"))
                || oStateModel.getProperty("/filterFailedChecks") !== "ALL"
                || oStateModel.getProperty("/filterFailedBarriers") !== "ALL";

            this.getView().getModel("view").setProperty("/hasActiveFilters", bHasFilters);
        },

        onSelect: function (oEvent) {
            var oCtx = oEvent.getParameter("listItem").getBindingContext("data");
            var oChecklist = oCtx.getObject();
            var sId = oChecklist && oChecklist.root ? oChecklist.root.id : "";

            if (!sId) {
                MessageToast.show("Checklist id not found");
                return;
            }

            this.getModel("selected").setData(oChecklist);
            this.getModel("state").setProperty("/layout", "TwoColumnsMidExpanded");
            this.navTo("detail", { id: sId });
        },

        onCreate: function () {
            this.getModel("state").setProperty("/layout", "TwoColumnsMidExpanded");
            this.navTo("object", { id: "__create" });
        },

        onCopy: function () {
            var oDataModel = this.getModel("data");
            var aVisible = oDataModel.getProperty("/visibleCheckLists") || [];
            var oFirst = aVisible[0];
            var sId = oFirst && oFirst.root ? oFirst.root.id : "";

            if (!sId) {
                MessageToast.show("Nothing to copy");
                return;
            }

            this.getModel("state").setProperty("/layout", "TwoColumnsMidExpanded");
            this.navTo("object", { id: sId });
        },

        onSearch: function () {
            var oDataModel = this.getModel("data");
            var oStateModel = this.getModel("state");
            var aSource = oDataModel.getProperty("/checkLists") || [];

            var sFilterId = (oStateModel.getProperty("/filterId") || "").trim().toLowerCase();
            var sFilterLpc = oStateModel.getProperty("/filterLpc") || "";
            var sFilterFailedChecks = oStateModel.getProperty("/filterFailedChecks") || "ALL";
            var sFilterFailedBarriers = oStateModel.getProperty("/filterFailedBarriers") || "ALL";

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

                return bIdMatch && bLpcMatch && bChecksMatch && bBarriersMatch;
            });

            oDataModel.setProperty("/visibleCheckLists", aFiltered);
            this._updateFilterState();
        },

        onResetFilters: function () {
            var oStateModel = this.getModel("state");

            oStateModel.setProperty("/filterId", "");
            oStateModel.setProperty("/filterLpc", "");
            oStateModel.setProperty("/filterFailedChecks", "ALL");
            oStateModel.setProperty("/filterFailedBarriers", "ALL");

            this.onSearch();
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
