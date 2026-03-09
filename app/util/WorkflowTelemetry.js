sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/util/DebugLogger",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TelemetryRuntime"
], function (DebugLogger, TelemetryRuntime) {
    "use strict";

    function emit(sEventName, mOptions) {
        var oOptions = mOptions || {};
        var oStateModel = oOptions.stateModel;
        var oPayload = Object.assign({
            sessionId: "",
            activeObjectId: "",
            mode: "READ",
            lockOperationState: "IDLE",
            timestamp: new Date().toISOString()
        }, TelemetryRuntime.workflowContextFromStateModel(oStateModel), oOptions.payload || {});

        DebugLogger.info("telemetry", sEventName, oPayload);
        return oPayload;
    }

    return {
        emit: emit
    };
});
