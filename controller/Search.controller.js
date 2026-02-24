sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator"
], function (Controller, Filter, FilterOperator) {
    "use strict";

    return Controller.extend("sap_ui5.controller.Search", {

        onInit: function () {
            const stateModel = this.getOwnerComponent().getModel("state");

            stateModel.setData(Object.assign({}, stateModel.getData(), {
                filterId: "",
                filterLpc: "",
                filterFailedChecks: "ALL",
                filterFailedBarriers: "ALL"
            }));
        },

        formatStatus: function (status) {
            switch (status) {
                case "SUCCESS": return "Success";
                case "WARNING": return "Warning";
                case "CRITICAL": return "Error";
                default: return "None";
            }
        },

        onSearch: function () {
            const oTable = this.byId("checkTable");
            const oBinding = oTable.getBinding("items");
            if (!oBinding) {
                return;
            }

            const state = this.getOwnerComponent().getModel("state").getData();

            const filters = [];

            if (state.filterId) {
                filters.push(new Filter("root/id", FilterOperator.Contains, state.filterId));
            }

            if (state.filterLpc) {
                filters.push(new Filter("basic/LPC_KEY", FilterOperator.EQ, state.filterLpc));
            }

            if (state.filterFailedChecks === "TRUE") {
                filters.push(new Filter("root/successRateChecks", FilterOperator.LT, 100));
            }

            if (state.filterFailedChecks === "FALSE") {
                filters.push(new Filter("root/successRateChecks", FilterOperator.EQ, 100));
            }

            if (state.filterFailedBarriers === "TRUE") {
                filters.push(new Filter("root/successRateBarriers", FilterOperator.LT, 100));
            }

            if (state.filterFailedBarriers === "FALSE") {
                filters.push(new Filter("root/successRateBarriers", FilterOperator.EQ, 100));
            }

            oBinding.filter(filters);
        },

        onSelect: function (oEvent) {
            const oListItem = oEvent.getParameter("listItem") || oEvent.getSource();
            const context = oListItem.getBindingContext("data");
            if (!context) {
                return;
            }
            const selected = context.getObject();

            this.getOwnerComponent()
                .getModel("data")
                .setProperty("/selectedChecklist", selected);

            this.getOwnerComponent()
                .getRouter()
                .navTo("detail", {
                    id: selected.root.id
                });
        },

        onCreate: function () {
            console.log("Create pressed");
        },

        onCopy: function () {
            console.log("Copy pressed");
        }

    });
});
