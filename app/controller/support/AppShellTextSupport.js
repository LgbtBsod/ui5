sap.ui.define([
    "checklist/app/controller/base/ControllerTextRuntime"
], function (ControllerTextRuntime) {
    "use strict";

    function getText(oController, sKey, aArgs, sFallback) {
        return ControllerTextRuntime.getText(oController, sKey, aArgs, sFallback || sKey);
    }

    return {
        getText: getText
    };
});

