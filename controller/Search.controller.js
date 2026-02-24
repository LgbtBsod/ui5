sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/m/MessageToast"
], function (Controller, MessageToast) {
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageToast"
], function (Controller, JSONModel, MessageToast) {
    "use strict";

    return Controller.extend("sap_ui5.controller.Search", {

        onSelect: function (oEvent) {
            const oItem = oEvent.getParameter("listItem");
            const oCtx = oItem.getBindingContext("data");
            const oChecklist = oCtx.getObject();
            const sId = oChecklist && oChecklist.root ? oChecklist.root.id : null;

            if (!sId) {
                MessageToast.show("Checklist id not found");
                return;
            }
        onInit: function () {
            const filterModel = new JSONModel({
                id: "",
                lpc: "ALL"
            });

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

            const aFiltered = aSource.filter((oItem) => {
                const sId = (((oItem || {}).root || {}).id || "").toLowerCase();
                const sLpc = (((oItem || {}).basic || {}).LPC || "");
                const nChecks = Number((((oItem || {}).root || {}).successRateChecks));
                const nBarriers = Number((((oItem || {}).root || {}).successRateBarriers));

                const bIdMatch = !sFilterId || sId.includes(sFilterId);
                const bLpcMatch = !sFilterLpc || sLpc === sFilterLpc;

                const bChecksFailed = Number.isFinite(nChecks) ? nChecks < 100 : false;
                const bBarriersFailed = Number.isFinite(nBarriers) ? nBarriers < 100 : false;

                const bChecksMatch = sFilterFailedChecks === "ALL"
                    || (sFilterFailedChecks === "TRUE" && bChecksFailed)
                    || (sFilterFailedChecks === "FALSE" && !bChecksFailed);

                const bBarriersMatch = sFilterFailedBarriers === "ALL"
                    || (sFilterFailedBarriers === "TRUE" && bBarriersFailed)
                    || (sFilterFailedBarriers === "FALSE" && !bBarriersFailed);

                return bIdMatch && bLpcMatch && bChecksMatch && bBarriersMatch;
            });

            oDataModel.setProperty("/visibleCheckLists", aFiltered);
        },

        onSelect: function (oEvent) {
            const ctx = oEvent.getParameter("listItem").getBindingContext("data");
            const data = ctx.getObject();

            this.getOwnerComponent().getModel("selected").setData(data);
        },

        onCreate: function () {
            MessageToast.show("Create action is not implemented yet");
        },

        onCopy: function () {
            MessageToast.show("Copy action is not implemented yet");
        },

        onSearch: function () {
            MessageToast.show("Filters are applied automatically");
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
