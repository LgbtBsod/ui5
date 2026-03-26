sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimePayloadNormalizer"
], function (RuntimePayloadNormalizer) {
    "use strict";

    function run(oController, sMethod, mInput) {
        var oFacade = oController && oController._facade;
        var oNormalizedInput = RuntimePayloadNormalizer.normalize(mInput || {});
        if (!oFacade || typeof oFacade[sMethod] !== "function") {
            return Promise.resolve(false);
        }
        if (typeof oController.executeFacadeMethod === "function") {
            return oController.executeFacadeMethod(oFacade, sMethod, oNormalizedInput, {});
        }
        return Promise.resolve(oFacade[sMethod](oNormalizedInput, {})).then(function (oResult) {
            if (typeof oController.applyUseCaseEffects === "function") {
                return oController.applyUseCaseEffects(oResult);
            }
            return oResult;
        });
    }

    return Object.freeze({
        analytics: function (oController, mInput) {
            return run(oController, "analytics", mInput);
        },
        bootstrap: function (oController, mInput) {
            return run(oController, "bootstrap", mInput);
        },
        buildFilter: function (oController, mInput) {
            return run(oController, "buildFilter", mInput);
        },
        executeSearch: function (oController, mInput) {
            return run(oController, "executeSearch", mInput);
        },
        exportFlow: function (oController, mInput) {
            return run(oController, "exportFlow", mInput);
        },
        rebind: function (oController, mInput) {
            return run(oController, "rebind", mInput);
        },
        selectRow: function (oController, mInput) {
            return run(oController, "selectRow", mInput);
        },
        selectionChanged: function (oController, mInput) {
            return run(oController, "selectionChanged", mInput);
        }
    });
});
