sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry"
], function (BehaviorRegistry) {
    "use strict";

    var DIALOG_SCOPE = "dialog";

    function ensureRegistered() {
        return true;
    }

    function register(sId, fnHandler) {
        return BehaviorRegistry.registerOverride(DIALOG_SCOPE, sId, fnHandler);
    }

    function unregister(sId) {
        return BehaviorRegistry.unregisterOverride(DIALOG_SCOPE, sId);
    }

    function clear() {
        return BehaviorRegistry.clearOverrides(DIALOG_SCOPE);
    }

    return {
        ensureRegistered: ensureRegistered,
        register: register,
        unregister: unregister,
        clear: clear
    };
});
