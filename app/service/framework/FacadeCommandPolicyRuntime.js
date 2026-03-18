sap.ui.define([], function () {
    "use strict";

    function buildPolicy(mOptions) {
        var fnExecute = mOptions && mOptions.execute;
        var aMethods = Array.isArray(mOptions && mOptions.methods) ? mOptions.methods.slice() : [];
        var oPolicy = {};

        function execute(oController, sMethod, mInput) {
            if (typeof fnExecute !== "function") {
                return Promise.resolve();
            }
            return fnExecute(oController, sMethod, mInput || {});
        }

        aMethods.forEach(function (sMethod) {
            oPolicy[sMethod] = function (oController, mInput) {
                return execute(oController, sMethod, mInput);
            };
        });

        return Object.freeze(oPolicy);
    }

    return {
        buildPolicy: buildPolicy
    };
});
