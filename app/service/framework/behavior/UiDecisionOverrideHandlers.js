sap.ui.define([
    "checklist/app/service/framework/behavior/BehaviorRegistry"
], function (BehaviorRegistry) {
    "use strict";

    var UI_DECISION_SCOPE = "uiDecision";

    function register(sId, fnHandler) {
        return BehaviorRegistry.registerOverride(UI_DECISION_SCOPE, sId, fnHandler);
    }

    function unregister(sId) {
        return BehaviorRegistry.unregisterOverride(UI_DECISION_SCOPE, sId);
    }

    function clear() {
        return BehaviorRegistry.clearOverrides(UI_DECISION_SCOPE);
    }

    function ensureRegistered() {
        return undefined;
    }

    return {
        register: register,
        unregister: unregister,
        clear: clear,
        ensureRegistered: ensureRegistered
    };
});
