sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageToast"
], function (Controller, JSONModel, MessageToast) {
    "use strict";

    return Controller.extend("sap_ui5.controller.Search", {

        onInit: function () {
            const filterModel = new JSONModel({
                id: "",
                lpc: "ALL"
            });

            this.getView().setModel(filterModel, "filters");
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
