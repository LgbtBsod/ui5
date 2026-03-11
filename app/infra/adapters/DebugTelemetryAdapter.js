sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/util/DebugLogger"
], function (DebugLogger) {
    "use strict";

    function emit(oEvent) {
        DebugLogger.info("telemetry", (oEvent && oEvent.event) || "event", oEvent || {});
        return oEvent || {};
    }

    return {
        emit: emit
    };
});
