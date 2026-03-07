sap.ui.define([
    "sap/base/Log"
], function (Log) {
    "use strict";

    function isEnabled() {
        return window.__DEBUG_UI5__ === true;
    }

    function info(sScope, sEvent, oPayload) {
        if (!isEnabled()) {
            return;
        }

        var sMessage = "[" + String(sScope || "UI5") + "] " + String(sEvent || "");
        var oSafePayload = oPayload || {};

        Log.info(sMessage, JSON.stringify(oSafePayload), "sap_ui5.util.DebugLogger");
    }

    return {
        isEnabled: isEnabled,
        info: info
    };
});
