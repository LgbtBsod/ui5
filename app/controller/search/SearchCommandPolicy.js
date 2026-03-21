sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerCommandRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimePayloadNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerCommandContextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/FacadeCommandConstants"
], function (ControllerCommandRuntime, RuntimePayloadNormalizer, ControllerCommandContextRuntime, FacadeCommandConstants) {
    "use strict";

    var COMMAND = FacadeCommandConstants.SEARCH;

    function normalizePayload(sMethod, mInput) {
        if (sMethod === COMMAND.APPLY_REBIND_POLICY) {
            return RuntimePayloadNormalizer.normalize(mInput, {
                booleanKeys: ["silent", "userInitiated"]
            });
        }
        return RuntimePayloadNormalizer.normalize(mInput);
    }

    function execute(oController, sMethod, mInput) {
        return ControllerCommandRuntime.executeFacadeCommand(
            oController,
            oController && oController._facade,
            sMethod,
            normalizePayload(sMethod, mInput || {}),
            ControllerCommandContextRuntime.buildSearchCtx(oController)
        );
    }

    return Object.freeze({
        buildFilter: function (oController, mInput) {
            return execute(oController, COMMAND.BUILD_FILTER, mInput);
        },
        executeSearch: function (oController, mInput) {
            return execute(oController, COMMAND.EXECUTE_SEARCH, mInput);
        },
        rebind: function (oController, mInput) {
            return execute(oController, COMMAND.REBIND, mInput);
        },
        selectRow: function (oController, mInput) {
            return execute(oController, COMMAND.SELECT_ROW, mInput);
        },
        selectionChanged: function (oController, mInput) {
            return execute(oController, COMMAND.SELECTION_CHANGED, mInput);
        },
        bootstrap: function (oController, mInput) {
            return execute(oController, COMMAND.BOOTSTRAP, mInput);
        },
        analytics: function (oController, mInput) {
            return execute(oController, COMMAND.ANALYTICS, mInput);
        },
        applyRebindPolicy: function (oController, mInput) {
            return execute(oController, COMMAND.APPLY_REBIND_POLICY, mInput);
        },
        exportFlow: function (oController, mInput) {
            return execute(oController, COMMAND.EXPORT_FLOW, mInput);
        }
    });
});
