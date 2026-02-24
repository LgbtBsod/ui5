sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel"
], function (Controller, JSONModel) {
    "use strict";

    return Controller.extend("sap_ui5.controller.Search", {

        onInit: function () {

            const data = [
                {
                    id: "CL-1001",
                    status: "SUCCESS",
                    checks: 100,
                    barriers: 100,
                    observer: "John Smith",
                    date: "2026-02-20",
                    equipment: "Pump 01",
                    lpc: "A",
                    checksList: [],
                    barriersList: []
                }
            ];

            const model = new JSONModel(data);
            this.getView().setModel(model, "checklists");

            const filterModel = new JSONModel({
                id: "",
                lpc: "ALL"
            });

            this.getView().setModel(filterModel, "filters");
        },

        onSelect: function (oEvent) {
            const ctx = oEvent.getParameter("listItem").getBindingContext("checklists");
            const data = ctx.getObject();

            const detailModel = new JSONModel(data);

            this.getOwnerComponent().getModel("selected").setData(data);
        },

        statusState: function (sStatus) {
            switch (sStatus) {
                case "SUCCESS": return "Success";
                case "WARNING": return "Warning";
                case "CRITICAL": return "Error";
                default: return "None";
            }
        }
    });
});