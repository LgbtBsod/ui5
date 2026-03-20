sap.ui.define([], function () {
    "use strict";

    function ensureRegistered(oDefaults, oOverrides) {
        if (oDefaults && typeof oDefaults.ensureRegistered === "function") {
            oDefaults.ensureRegistered();
        }
        if (oOverrides && typeof oOverrides.ensureRegistered === "function") {
            oOverrides.ensureRegistered();
        }
    }

    function create(mConfig) {
        var sScope = mConfig && mConfig.scope;
        var oResolver = mConfig && mConfig.resolver;
        var oDefaults = mConfig && mConfig.defaultHandlers;
        var oOverrides = mConfig && mConfig.overrideHandlers;

        function execute(sOperation, mContext) {
            ensureRegistered(oDefaults, oOverrides);
            return oResolver.execute(sScope, sOperation, mContext || {}, oDefaults && oDefaults.handlers);
        }

        function executeSync(sOperation, mContext) {
            ensureRegistered(oDefaults, oOverrides);
            return oResolver.executeSync(sScope, sOperation, mContext || {}, oDefaults && oDefaults.handlers);
        }

        return Object.freeze({
            execute: execute,
            executeSync: executeSync,
            registerBehaviorOverride: oOverrides && oOverrides.register,
            unregisterBehaviorOverride: oOverrides && oOverrides.unregister,
            clearBehaviorOverrides: oOverrides && oOverrides.clear
        });
    }

    return Object.freeze({
        create: create
    });
});
