sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry"
], function (BehaviorRegistry) {
    "use strict";

    var RETRY_SCOPE = "retry";

    return {
        ensureRegistered: function () {
            return true;
        },
        register: function (sId, fnHandler) {
            return BehaviorRegistry.registerOverride(RETRY_SCOPE, sId, fnHandler);
        },
        unregister: function (sId) {
            return BehaviorRegistry.unregisterOverride(RETRY_SCOPE, sId);
        },
        clear: function () {
            return BehaviorRegistry.clearOverrides(RETRY_SCOPE);
        }
    };
});
