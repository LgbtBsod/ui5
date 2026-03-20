sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRuntimeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry"
], function (DetailRuntimeConstants, BehaviorRegistry) {
    "use strict";

    var bDefaultsRegistered = false;

    function resolveAnalyticsEditRestorePlan() {
        return {
            maxAttempts: 3,
            retryDelayMs: 220
        };
    }

    var mHandlers = {};
    mHandlers[DetailRuntimeConstants.OP_ANALYTICS_EDIT_RESTORE] = resolveAnalyticsEditRestorePlan;

    function ensureRegistered() {
        if (bDefaultsRegistered) {
            return;
        }
        Object.keys(mHandlers).forEach(function (sOperation) {
            BehaviorRegistry.registerDefault(DetailRuntimeConstants.SCOPE, sOperation, mHandlers[sOperation]);
        });
        bDefaultsRegistered = true;
    }

    return {
        handlers: mHandlers,
        ensureRegistered: ensureRegistered
    };
});
