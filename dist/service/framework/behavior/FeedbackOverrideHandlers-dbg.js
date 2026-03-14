sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry"
], function (BehaviorRegistry) {
    "use strict";

    var FEEDBACK_SCOPE = "feedback";

    return {
        ensureRegistered: function () {
            return true;
        },
        register: function (sId, fnHandler) {
            return BehaviorRegistry.registerOverride(FEEDBACK_SCOPE, sId, fnHandler);
        },
        unregister: function (sId) {
            return BehaviorRegistry.unregisterOverride(FEEDBACK_SCOPE, sId);
        },
        clear: function () {
            return BehaviorRegistry.clearOverrides(FEEDBACK_SCOPE);
        }
    };
});
