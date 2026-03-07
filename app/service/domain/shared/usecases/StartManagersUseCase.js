sap.ui.define([
    "sap_ui5/service/framework/Result"
], function (Result) {
    "use strict";

    function startIf(oMgr, fnGuard) {
        if (!oMgr) { return; }
        if (fnGuard && !fnGuard(oMgr)) { return; }
        oMgr.start();
    }

    return {
        execute: function (input, ctx) {
            var m = ctx.managers || {};
            var bLockActive = !!(input && input.lockRuntimeActive);
            if (input && input.scope === "core") {
                startIf(m.heartbeat);
                startIf(m.activity);
                startIf(m.autosave);
                startIf(m.connectivity);
                startIf(m.lockStatus);
            }
            if (input && input.scope === "lock") {
                if (bLockActive) {
                    startIf(m.heartbeat, function (o) { return !o.isRunning || !o.isRunning(); });
                    startIf(m.autosave);
                    startIf(m.lockStatus);
                    if (m.gcd && m.gcd.resetOnFullSave) { m.gcd.resetOnFullSave(); }
                    startIf(m.activity);
                } else {
                    [m.heartbeat, m.autosave, m.lockStatus, m.activity].forEach(function (o) { if (o && o.stop) { o.stop(); } });
                    if (m.gcd && m.gcd.destroyManager) { m.gcd.destroyManager(); }
                }
            }
            return Promise.resolve(Result.ok({ started: true }, []));
        }
    };
});
