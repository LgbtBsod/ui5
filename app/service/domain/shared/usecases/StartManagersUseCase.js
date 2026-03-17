sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result"
], function (Result) {
    "use strict";

    function startIf(oMgr, fnGuard) {
        if (!oMgr) { return; }
        if (fnGuard && !fnGuard(oMgr)) { return; }
        oMgr.start();
    }

    function stopIf(oMgr, sMethod) {
        var sStopMethod = sMethod || "stop";
        if (oMgr && typeof oMgr[sStopMethod] === "function") {
            oMgr[sStopMethod]();
        }
    }

    return {
        execute: function (input, ctx) {
            var m = ctx.managers || {};
            var bLockActive = !!(input && input.lockRuntimeActive);
            if (input && input.scope === "core") {
                if (m.gcd && m.gcd.resetOnFullSave) {
                    m.gcd.resetOnFullSave();
                }
            }
            if (input && input.scope === "lock") {
                if (bLockActive) {
                    startIf(m.heartbeat, function (o) { return !o.isRunning || !o.isRunning(); });
                    startIf(m.autosave);
                    startIf(m.lockStatus);
                    startIf(m.gcd, function (o) { return !o.isRunning || !o.isRunning(); });
                    startIf(m.activity);
                } else {
                    [m.heartbeat, m.autosave, m.lockStatus, m.activity, m.gcd].forEach(function (o) { stopIf(o); });
                }
            }
            return Promise.resolve(Result.ok({ started: true }, []));
        }
    };
});
