sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageToast"
], function (Controller, JSONModel, MessageToast) {
    "use strict";

    return Controller.extend("sap_ui5.controller.Search", {

        onInit: function () {
            const oStateModel = this.getOwnerComponent().getModel("state");
            const oViewModel = new JSONModel({
                hasActiveFilters: false
            });

            this.getView().setModel(oViewModel, "view");

            ["/filterId", "/filterLpc", "/filterFailedChecks", "/filterFailedBarriers"].forEach((sPath) => {
                oStateModel.bindProperty(sPath).attachChange(this._updateFilterState, this);
            });

            this._updateFilterState();
        },

        _updateFilterState: function () {
            const oStateModel = this.getOwnerComponent().getModel("state");
            const bHasFilters = Boolean((oStateModel.getProperty("/filterId") || "").trim())
                || Boolean(oStateModel.getProperty("/filterLpc"))
                || oStateModel.getProperty("/filterFailedChecks") !== "ALL"
                || oStateModel.getProperty("/filterFailedBarriers") !== "ALL";

            this.getView().getModel("view").setProperty("/hasActiveFilters", bHasFilters);
        },

        onSelect: function (oEvent) {
            const oCtx = oEvent.getParameter("listItem").getBindingContext("data");
            const oChecklist = oCtx.getObject();
            const sId = oChecklist && oChecklist.root ? oChecklist.root.id : "";

            if (!sId) {
                MessageToast.show("Checklist id not found");
                return;
            }

            this.getOwnerComponent().getModel("selected").setData(oChecklist);
            this.getOwnerComponent().getModel("state").setProperty("/layout", "TwoColumnsMidExpanded");
            this.getOwnerComponent().getRouter().navTo("detail", { id: sId });
        },

        onCreate: function () {
            MessageToast.show("Create action is not implemented yet");
        },

        onCopy: function () {
            MessageToast.show("Copy action is not implemented yet");
        },

        onSearch: function () {
            const oComponent = this.getOwnerComponent();
            const oDataModel = oComponent.getModel("data");
            const oStateModel = oComponent.getModel("state");
            const aSource = oDataModel.getProperty("/checkLists") || [];

            const sFilterId = (oStateModel.getProperty("/filterId") || "").trim().toLowerCase();
            const sFilterLpc = oStateModel.getProperty("/filterLpc") || "";
            const sFilterFailedChecks = oStateModel.getProperty("/filterFailedChecks") || "ALL";
            const sFilterFailedBarriers = oStateModel.getProperty("/filterFailedBarriers") || "ALL";

            const aFiltered = aSource.filter(function (oItem) {
                const sId = (((oItem || {}).root || {}).id || "").toLowerCase();
                const sLpc = (((oItem || {}).basic || {}).LPC_KEY || "");
                const nChecks = Number((((oItem || {}).root || {}).successRateChecks));
                const nBarriers = Number((((oItem || {}).root || {}).successRateBarriers));

                const bIdMatch = !sFilterId || sId.includes(sFilterId);
                const bLpcMatch = !sFilterLpc || sLpc === sFilterLpc;

                const bChecksFailed = Number.isFinite(nChecks) && nChecks < 100;
                const bBarriersFailed = Number.isFinite(nBarriers) && nBarriers < 100;

                const bChecksMatch = sFilterFailedChecks === "ALL"
                    || (sFilterFailedChecks === "TRUE" && bChecksFailed)
                    || (sFilterFailedChecks === "FALSE" && !bChecksFailed);

                const bBarriersMatch = sFilterFailedBarriers === "ALL"
                    || (sFilterFailedBarriers === "TRUE" && bBarriersFailed)
                    || (sFilterFailedBarriers === "FALSE" && !bBarriersFailed);

                return bIdMatch && bLpcMatch && bChecksMatch && bBarriersMatch;
            });

            oDataModel.setProperty("/visibleCheckLists", aFiltered);
            this._updateFilterState();
        },

        onResetFilters: function () {
            const oStateModel = this.getOwnerComponent().getModel("state");

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
