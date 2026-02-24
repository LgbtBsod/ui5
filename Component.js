sap.ui.define([
    "sap/ui/core/UIComponent",
    "sap/ui/Device",
    "sap_ui5/model/ModelFactory",
    "sap_ui5/service/ChecklistService",
    "sap/ui/model/json/JSONModel"
], function (UIComponent, Device, ModelFactory, ChecklistService, JSONModel) {
    "use strict";

    return UIComponent.extend("sap_ui5.Component", {

        metadata: {
            manifest: "json"
        },

        init: function () {
            // 1. Инициализация родителя
            UIComponent.prototype.init.apply(this, arguments);

            // 2. Создание базовых моделей
            const oDataModel = ModelFactory.createDataModel();
            const oStateModel = ModelFactory.createStateModel();
            const oReferenceModel = ModelFactory.createReferenceModel();

            this.setModel(new JSONModel({}), "selected");
            this.setModel(oDataModel, "data");
            this.setModel(oStateModel, "state");
            this.setModel(oReferenceModel, "ref");

            // 3. Запуск роутера (важно делать это СРАЗУ, чтобы вьюхи начали грузиться)
            this.getRouter().initialize();

            // 4. Загрузка данных
            oStateModel.setProperty("/isLoading", true);

            Promise.all([
                ChecklistService.loadCheckLists().catch(() => []),
                ChecklistService.loadPersons().catch(() => []),
                ChecklistService.loadLpc().catch(() => []),
                ChecklistService.loadProfessions().catch(() => []),
                ChecklistService.loadLocations().catch(() => [])
            ]).then(([checkLists, persons, lpc, professions, locations]) => {
                
                oDataModel.setProperty("/checkLists", checkLists);
                oDataModel.setProperty("/visibleCheckLists", checkLists);
                
                oReferenceModel.setProperty("/persons", persons);
                oReferenceModel.setProperty("/lpc", lpc);
                oReferenceModel.setProperty("/professions", professions);
                oReferenceModel.setProperty("/locations", locations);

            }).catch((oError) => {
                oStateModel.setProperty("/loadError", true);
                oStateModel.setProperty("/loadErrorMessage", "Ошибка при загрузке данных: " + oError.message);
            }).finally(() => {
                oStateModel.setProperty("/isLoading", false);
            });
        }
    });
});