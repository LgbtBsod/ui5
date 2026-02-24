sap.ui.define([
    "sap_ui5/service/backend/FakeBackendService"
], function (FakeBackendService) {
    "use strict";

    return {

        login: function (username) {
            return FakeBackendService.login(username);
        },

        init: function () {
            return FakeBackendService.init();
        },

        getCheckLists: function () {
            return FakeBackendService.getCheckLists();
        },

        createCheckList: function (oData) {
            return FakeBackendService.createCheckList(oData);
        },

        updateCheckList: function (sId, oData) {
            return FakeBackendService.updateCheckList(sId, oData);
        },

        createObject: function (data) {
            return FakeBackendService.create(data);
        },

        readObject: function (uuid) {
            return FakeBackendService.read(uuid);
        },

        updateObject: function (uuid, data) {
            return FakeBackendService.update(uuid, data);
        },

        getAllObjects: function () {
            return FakeBackendService.getAll();
        }

    };
});
