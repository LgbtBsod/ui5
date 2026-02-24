sap.ui.define([
    "ui5/services/backend/FakeBackendService"
], function (FakeBackendService) {
    "use strict";

    return {

        login: function (username) {
            return FakeBackendService.login(username);
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