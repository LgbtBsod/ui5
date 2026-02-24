sap.ui.define([
    "sap/ui/core/UIComponent",
    "sap_ui5/model/ModelFactory",
    "sap_ui5/service/ChecklistService",
    "sap_ui5/service/backend/BackendAdapter",
    "sap/ui/model/json/JSONModel"
], function (UIComponent, ModelFactory, ChecklistService, BackendAdapter, JSONModel) {
    "use strict";

    return UIComponent.extend("sap_ui5.Component", {

        metadata: {
            manifest: "json"
        },

        init: function () {
            UIComponent.prototype.init.apply(this, arguments);

            var oDataModel = ModelFactory.createDataModel();
            var oStateModel = ModelFactory.createStateModel();
            var oReferenceModel = ModelFactory.createReferenceModel();

            this.setModel(new JSONModel({}), "selected");
            this.setModel(oDataModel, "data");
            this.setModel(oStateModel, "state");
            this.setModel(oReferenceModel, "ref");

            this.getRouter().initialize();

            oStateModel.setProperty("/isLoading", true);

            Promise.all([
                BackendAdapter.login("demoUser"),
                BackendAdapter.init(),
                ChecklistService.loadPersons().catch(function () { return []; }),
                ChecklistService.loadLpc().catch(function () { return []; }),
                ChecklistService.loadProfessions().catch(function () { return []; }),
                ChecklistService.loadLocations().catch(function () { return []; })
            ]).then(function (aResults) {
                var oLogin = aResults[0];
                var persons = aResults[2];
                var lpc = aResults[3];
                var professions = aResults[4];
                var locations = aResults[5];

                oStateModel.setProperty("/sessionId", oLogin.sessionId);

                return BackendAdapter.getCheckLists().then(function (checkLists) {
                    oDataModel.setProperty("/checkLists", checkLists);
                    oDataModel.setProperty("/visibleCheckLists", []);
                }).then(function () {
                    oReferenceModel.setProperty("/persons", persons);
                    oReferenceModel.setProperty("/lpc", lpc);
                    oReferenceModel.setProperty("/professions", professions);
                    oReferenceModel.setProperty("/locations", locations);
                });
            }).catch(function (oError) {
                oStateModel.setProperty("/loadError", true);
                oStateModel.setProperty("/loadErrorMessage", "Ошибка при загрузке данных: " + oError.message);
            }).finally(function () {
                oStateModel.setProperty("/isLoading", false);
            });
        }
    });
});
