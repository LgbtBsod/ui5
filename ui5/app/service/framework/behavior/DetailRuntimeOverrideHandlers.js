sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DetailRuntimeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry"
], function (DetailRuntimeConstants, BehaviorRegistry) {
    "use strict";

    return {
        ensureRegistered: function () {
            return true;
        },
        register: function (sId, fnHandler) {
            return BehaviorRegistry.registerOverride(DetailRuntimeConstants.SCOPE, sId, fnHandler);
        },
        unregister: function (sId) {
            return BehaviorRegistry.unregisterOverride(DetailRuntimeConstants.SCOPE, sId);
        },
        clear: function () {
            return BehaviorRegistry.clearOverrides(DetailRuntimeConstants.SCOPE);
        }
    };
});
