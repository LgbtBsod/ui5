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

            oStateModel.setProperty("/isLoading", true);
            oStateModel.setProperty("/loadError", false);
            oStateModel.setProperty("/loadErrorMessage", "");

            Promise.all([
                ChecklistService.loadCheckLists(),
                ChecklistService.loadPersons(),
                ChecklistService.loadLpc(),
                ChecklistService.loadProfessions(),
                ChecklistService.loadLocations()
            ]).then(([checkLists, persons, lpc, professions, locations]) => {

                oDataModel.setProperty("/checkLists", Array.isArray(checkLists) ? checkLists : []);
                oReferenceModel.setProperty("/persons", Array.isArray(persons) ? persons : []);
                oReferenceModel.setProperty("/lpc", Array.isArray(lpc) ? lpc : []);
                oReferenceModel.setProperty("/professions", Array.isArray(professions) ? professions : []);
                oReferenceModel.setProperty("/locations", Array.isArray(locations) ? locations : []);

            }).catch(function (oError) {
                oDataModel.setProperty("/checkLists", []);
                oReferenceModel.setProperty("/persons", []);
                oReferenceModel.setProperty("/lpc", []);
                oReferenceModel.setProperty("/professions", []);
                oReferenceModel.setProperty("/locations", []);

                oStateModel.setProperty("/loadError", true);
                oStateModel.setProperty("/loadErrorMessage", oError && oError.message ? oError.message : "Unknown loading error");
            }).finally(function () {
                oStateModel.setProperty("/isLoading", false);
            });

            this.getRouter().initialize();
        }

    });

});
