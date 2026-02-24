sap.ui.define([
    "sap_ui5/service/backend/InMemoryDB",
    "sap_ui5/service/backend/FakeODataService"
], function (InMemoryDB, FakeODataService) {
    "use strict";

    function _loadInitialChecklists() {
        return fetch("mock/checklists.json")
            .then(function (oResponse) {
                if (!oResponse.ok) {
                    throw new Error("Failed to load mock/checklists.json");
                }
                return oResponse.json();
            })
            .then(function (oData) {
                return oData && Array.isArray(oData.check_lists) ? oData.check_lists : [];
            });
    }

    return {

        login: function (username) {
            return Promise.resolve({
                sessionId: "sess-" + Date.now(),
                user: username
            });
        },

        init: function () {
            return _loadInitialChecklists().then(function (aChecklists) {
                InMemoryDB.init(aChecklists);
                return aChecklists;
            });
        },

        getCheckLists: function () {
            return Promise.resolve(InMemoryDB.getCheckLists());
        },

        queryCheckLists: function (mQuery) {
            return FakeODataService.readEntitySet("CheckLists", mQuery).then(function (oResult) {
                return oResult.results || [];
            });
        },

        createCheckList: function (oData) {
            return Promise.resolve(InMemoryDB.createCheckList(oData));
        },

        updateCheckList: function (sId, oData) {
            return Promise.resolve(InMemoryDB.updateCheckList(sId, oData));
        },

        deleteCheckList: function (sId) {
            return Promise.resolve(InMemoryDB.deleteCheckList(sId));
        },

        upsertRows: function (sId, sSection, aRows) {
            return Promise.resolve(InMemoryDB.upsertRows(sId, sSection, aRows));
        },

        lockHeartbeat: function (sSessionId) {
            return FakeODataService.callFunctionImport("LockHeartbeat", { sessionId: sSessionId });
        },

        lockRelease: function (sSessionId) {
            return FakeODataService.callFunctionImport("LockRelease", { sessionId: sSessionId });
        },

        getServerState: function () {
            return FakeODataService.callFunctionImport("ServerState", {});
        },

        create: function (payload) {
            return Promise.resolve(InMemoryDB.createObject(payload));
        },

        read: function (uuid) {
            return Promise.resolve(InMemoryDB.readObject(uuid));
        },

        update: function (uuid, payload) {
            return Promise.resolve(InMemoryDB.updateObject(uuid, payload));
        },

        getAll: function () {
            return Promise.resolve(InMemoryDB.getAll());
        }

    };
});
