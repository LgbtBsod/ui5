sap.ui.define([
    "sap_ui5/controller/base/ControllerTextRuntime"
], function (ControllerTextRuntime) {
    "use strict";

    function getText(oController, sKey, aArgs, sFallback) {
        return ControllerTextRuntime.getText(oController, sKey, aArgs, sFallback || sKey);
    }

    return {
        getText: getText
    };
});

