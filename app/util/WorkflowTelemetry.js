sap.ui.define([
    "checklist/app/util/DebugLogger"
], function (DebugLogger) {
    "use strict";

    function read(oStateModel, sPath, vFallback) {
        if (!oStateModel || !oStateModel.getProperty) {
            return vFallback;
        }
        var vValue = oStateModel.getProperty(sPath);
        return vValue === undefined ? vFallback : vValue;
    }

    function emit(sEventName, mOptions) {
        var oOptions = mOptions || {};
        var oStateModel = oOptions.stateModel;
        var oPayload = Object.assign({
            sessionId: read(oStateModel, "/sessionId", ""),
            activeObjectId: read(oStateModel, "/activeObjectId", ""),
            mode: read(oStateModel, "/mode", "READ"),
            lockOperationState: read(oStateModel, "/lockOperationState", "IDLE"),
            timestamp: new Date().toISOString()
        }, oOptions.payload || {});

        DebugLogger.info("telemetry", sEventName, oPayload);
        return oPayload;
    }

    return {
        emit: emit
    };
});
