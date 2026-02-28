sap.ui.define([], function () {
    "use strict";

    var _loaded = false;
    var _subscribers = [];

    function _notify(oRuntime) {
        _subscribers.forEach(function (fn) {
            try { fn(oRuntime || {}); } catch (e) { /* noop */ }
        });
    }

    return {
        subscribe: function (fnHandler) {
            if (typeof fnHandler === "function") {
                _subscribers.push(fnHandler);
            }
        },

        load: function (gatewayClient, masterDataModel) {
            if (_loaded) {
                return Promise.resolve(masterDataModel.getProperty("/runtime") || {});
            }
            return gatewayClient.readEntity("RuntimeSettingsSet", "Key='GLOBAL'", {}).then(function (oData) {
                masterDataModel.setProperty("/runtime", oData || {});
                _loaded = true;
                _notify(oData || {});
                return oData || {};
            });
        }
    };
});
