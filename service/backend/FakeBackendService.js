sap.ui.define([
    "sap_ui5/service/backend/InMemoryDB"
], function (InMemoryDB) {
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
