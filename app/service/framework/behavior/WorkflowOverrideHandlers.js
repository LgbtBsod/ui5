sap.ui.define([
    "checklist/app/service/framework/behavior/BehaviorRegistry"
], function (BehaviorRegistry) {
    "use strict";

    var WORKFLOW_SCOPE = "workflow";

    return {
        ensureRegistered: function () {
            return true;
        },
        register: function (sId, fnHandler) {
            return BehaviorRegistry.registerOverride(WORKFLOW_SCOPE, sId, fnHandler);
        },
        unregister: function (sId) {
            return BehaviorRegistry.unregisterOverride(WORKFLOW_SCOPE, sId);
        },
        clear: function () {
            return BehaviorRegistry.clearOverrides(WORKFLOW_SCOPE);
        }
    };
});
