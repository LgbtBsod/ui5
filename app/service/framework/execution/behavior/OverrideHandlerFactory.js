sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry"
], function (BehaviorRegistry) {
    "use strict";

    function create(sScope) {
        return Object.freeze({
            ensureRegistered: function () {
                return undefined;
            },
            register: function (sOperation, fnHandler, sBehaviorId) {
                return BehaviorRegistry.registerOverride(sScope, sOperation, fnHandler, sBehaviorId);
            },
            unregister: function (sOperation, sBehaviorId) {
                return BehaviorRegistry.unregisterOverride(sScope, sOperation, sBehaviorId);
            },
            clear: function () {
                return BehaviorRegistry.clearOverrides(sScope);
            }
        });
    }

    return Object.freeze({
        create: create
    });
});
