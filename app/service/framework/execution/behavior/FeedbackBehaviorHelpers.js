sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime"
], function (ControllerTextRuntime) {
    "use strict";

    function resolveText(oController, sTextKey, aArgs, sFallback) {
        return ControllerTextRuntime.getText(
            oController,
            sTextKey,
            aArgs || [],
            sFallback || sTextKey || ""
        );
    }

    function createResolver(oController) {
        return function (sKey, aArgs, sFallback) {
            return resolveText(oController, sKey, aArgs || [], sFallback || sKey);
        };
    }

    return Object.freeze({
        resolveText: resolveText,
        createResolver: createResolver
    });
});
