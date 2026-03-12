sap.ui.define(["PRODUCTION_CONTROL_CHECKLIST/infra/adapters/LockAdapter"], function (LockAdapter) {
    "use strict";

    function release(sObjectId, sSessionId) {
        if (!sObjectId || !sSessionId) {
            return Promise.resolve();
        }
        return LockAdapter.release({ rootId: sObjectId, sessionGuid: sSessionId }).catch(function () { return null; });
    }

    return { release: release };
});
