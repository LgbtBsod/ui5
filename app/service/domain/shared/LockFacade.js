sap.ui.define(["PRODUCTION_CONTROL_CHECKLIST/infra/adapters/LockAdapter"], function (LockAdapter) {
    "use strict";

    var oLockPort = LockAdapter.create();

    function release(sObjectId, sSessionId) {
        if (!sObjectId || !sSessionId) {
            return Promise.resolve();
        }
        return oLockPort.release({ rootId: sObjectId, sessionGuid: sSessionId }).catch(function () { return null; });
    }

    return { release: release };
});
