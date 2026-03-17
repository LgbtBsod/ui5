sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UiBehaviorConstants"
], function (BehaviorRegistry, UiBehaviorConstants) {
    "use strict";

    return {
        ensureRegistered: function () {
            return true;
        },
        register: function (sId, fnHandler) {
            return BehaviorRegistry.registerOverride(UiBehaviorConstants.SCOPE, sId, fnHandler);
        },
        unregister: function (sId) {
            return BehaviorRegistry.unregisterOverride(UiBehaviorConstants.SCOPE, sId);
        },
        clear: function () {
            return BehaviorRegistry.clearOverrides(UiBehaviorConstants.SCOPE);
        }
    };
});
