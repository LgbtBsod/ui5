sap.ui.define([
    "sap_ui5/service/backend/InMemoryDB"
], function (InMemoryDB) {
    "use strict";

    return {
        readEntitySet: function (sEntitySet, mQuery) {
            if (sEntitySet === "CheckLists") {
                return Promise.resolve({ results: InMemoryDB.queryCheckLists(mQuery || {}) });
            }

            return Promise.resolve({ results: [] });
        },

        callFunctionImport: function (sName, mParams) {
            if (sName === "LockHeartbeat") {
                return Promise.resolve(InMemoryDB.lockHeartbeat((mParams || {}).sessionId));
            }

            if (sName === "LockRelease") {
                return Promise.resolve(InMemoryDB.lockRelease((mParams || {}).sessionId));
            }

            if (sName === "ServerState") {
                return Promise.resolve(InMemoryDB.getServerState());
            }

            return Promise.resolve({});
        }
    };
});
