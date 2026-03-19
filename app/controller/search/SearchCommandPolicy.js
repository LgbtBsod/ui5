sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerCommandRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimePayloadNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerCommandContextRuntime"
], function (ControllerCommandRuntime, RuntimePayloadNormalizer, ControllerCommandContextRuntime) {
    "use strict";

    function normalizePayload(mInput) {
        return RuntimePayloadNormalizer.normalize(mInput);
    }

    function execute(oController, sMethod, mInput) {
        return ControllerCommandRuntime.executeFacadeCommand(
            oController,
            oController && oController._facade,
            sMethod,
            normalizePayload(mInput || {}),
            ControllerCommandContextRuntime.buildSearchCtx(oController)
        );
    }

    return Object.freeze({
        buildFilter: function (oController, mInput) {
            return execute(oController, "buildFilter", mInput);
        },
        executeSearch: function (oController, mInput) {
            return execute(oController, "executeSearch", mInput);
        },
        rebind: function (oController, mInput) {
            return execute(oController, "rebind", mInput);
        },
        selectRow: function (oController, mInput) {
            return execute(oController, "selectRow", mInput);
        },
        selectionChanged: function (oController, mInput) {
            return execute(oController, "selectionChanged", mInput);
        },
        bootstrap: function (oController, mInput) {
            return execute(oController, "bootstrap", mInput);
        },
        analytics: function (oController, mInput) {
            return execute(oController, "analytics", mInput);
        },
        applyRebindPolicy: function (oController, mInput) {
            return execute(oController, "applyRebindPolicy", mInput);
        },
        exportFlow: function (oController, mInput) {
            return execute(oController, "exportFlow", mInput);
        }
    });
});
