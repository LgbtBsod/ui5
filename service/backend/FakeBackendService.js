sap.ui.define([
    "ui5/services/backend/InMemoryDB"
], function (InMemoryDB) {
    "use strict";

    return {

        login: function (username) {
            return Promise.resolve({
                sessionId: "sess-" + Date.now(),
                user: username
            });
        },

        create: function (payload) {
            var result = InMemoryDB.createObject(payload);
            return Promise.resolve(result);
        },

        read: function (uuid) {
            var result = InMemoryDB.readObject(uuid);
            return Promise.resolve(result);
        },

        update: function (uuid, payload) {
            var result = InMemoryDB.updateObject(uuid, payload);
            return Promise.resolve(result);
        },

        getAll: function () {
            return Promise.resolve(InMemoryDB.getAll());
        }

    };
});