sap.ui.define([
    "sap_ui5/service/backend/FakeBackendService"
], function (FakeBackendService) {
    "use strict";

    // Integration note:
    // current UI runs against a fake backend for deterministic frontend development.
    // planned productive backend: OData V2 service on SAP BASIS 750 SP15 + SAP HANA 2 SP7,
    // with CDS-based entities as canonical data model.
    var BackendService = FakeBackendService;

    return {

        login: function (username) {
            return BackendService.login(username);
        },

        init: function () {
            return BackendService.init();
        },

        getCheckLists: function () {
            return BackendService.getCheckLists();
        },

        createCheckList: function (oData) {
            return BackendService.createCheckList(oData);
        },

        updateCheckList: function (sId, oData) {
            return BackendService.updateCheckList(sId, oData);
        },

        deleteCheckList: function (sId) {
            return BackendService.deleteCheckList(sId);
        },

        upsertRows: function (sId, sSection, aRows) {
            return BackendService.upsertRows(sId, sSection, aRows);
        },

        createObject: function (data) {
            return BackendService.create(data);
        },

        readObject: function (uuid) {
            return BackendService.read(uuid);
        },

        updateObject: function (uuid, data) {
            return BackendService.update(uuid, data);
        },

        getAllObjects: function () {
            return BackendService.getAll();
        }

    };
});
