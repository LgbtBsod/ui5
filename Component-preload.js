sap.ui.define([
    "sap/ui/core/UIComponent",
    "sap/ui/Device",
    "sap_ui5/model/ModelFactory",
    "sap_ui5/service/ChecklistService"
], function (UIComponent, Device, ModelFactory, ChecklistService) {
    "use strict";

    return UIComponent.extend("sap_ui5.Component", {

        metadata: {
            manifest: "json"
        },

        init: function () {
            UIComponent.prototype.init.apply(this, arguments);

            const oDataModel = ModelFactory.createDataModel();
            const oStateModel = ModelFactory.createStateModel();
            const oReferenceModel = ModelFactory.createReferenceModel();

            this.setModel(oDataModel, "data");
            this.setModel(oStateModel, "state");
            this.setModel(oReferenceModel, "ref");

            Promise.all([
                ChecklistService.loadCheckLists(),
                ChecklistService.loadPersons(),
                ChecklistService.loadLpc(),
                ChecklistService.loadProfessions(),
                ChecklistService.loadLocations()
            ]).then(([checkLists, persons, lpc, professions, locations]) => {

                oDataModel.setProperty("/checkLists", checkLists);
                oReferenceModel.setProperty("/persons", persons);
                oReferenceModel.setProperty("/lpc", lpc);
                oReferenceModel.setProperty("/professions", professions);
                oReferenceModel.setProperty("/locations", locations);

            });

            this.getRouter().initialize();
        }

    });
});