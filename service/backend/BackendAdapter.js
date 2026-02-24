sap.ui.define([
    "sap_ui5/service/backend/FakeBackendService"
], function (FakeBackendService) {
    "use strict";

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

        queryCheckLists: function (mQuery) {
            return BackendService.queryCheckLists(mQuery);
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

        lockHeartbeat: function (sSessionId) {
            return BackendService.lockHeartbeat(sSessionId);
        },

        lockRelease: function (sSessionId) {
            return BackendService.lockRelease(sSessionId);
        },

        getServerState: function () {
            return BackendService.getServerState();
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
