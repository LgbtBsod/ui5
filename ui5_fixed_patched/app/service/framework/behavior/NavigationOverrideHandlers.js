sap.ui.define([
    "checklist/app/service/framework/behavior/BehaviorRegistry"
], function (BehaviorRegistry) {
    "use strict";

    var NAVIGATION_SCOPE = "navigation";

    return {
        ensureRegistered: function () {
            return true;
        },
        register: function (sId, fnHandler) {
            return BehaviorRegistry.registerOverride(NAVIGATION_SCOPE, sId, fnHandler);
        },
        unregister: function (sId) {
            return BehaviorRegistry.unregisterOverride(NAVIGATION_SCOPE, sId);
        },
        clear: function () {
            return BehaviorRegistry.clearOverrides(NAVIGATION_SCOPE);
        }
    };
});
