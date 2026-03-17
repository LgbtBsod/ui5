sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/EditSessionRuntime"
], function (Result, EditSessionRuntime) {
    "use strict";

    return {
        execute: function (input, ctx) {
            var m = ctx.managers || {};
            var bLockActive = !!(input && input.lockRuntimeActive);
            if (input && input.scope === "core") {
                EditSessionRuntime.resetAfterWrite(m);
            }
            if (input && input.scope === "lock") {
                if (bLockActive) {
                    EditSessionRuntime.startLockScoped(m);
                } else {
                    EditSessionRuntime.stopLockScoped(m);
                }
            }
            return Promise.resolve(Result.ok({ started: true }, []));
        }
    };
});
