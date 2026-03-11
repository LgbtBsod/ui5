sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/CorrelationId",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/util/telemetry/MemoryTelemetryBuffer",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/DebugTelemetryAdapter"
], function (CorrelationId, ModelStateRuntime, TelemetryRuntime, MemoryTelemetryBuffer, DebugTelemetryAdapter) {
    "use strict";

    function buildEvent(sEventName, mOptions) {
        var oOptions = mOptions || {};
        var oStateModel = oOptions.stateModel;
        var oPayload = Object.assign({
            sessionId: "",
            activeObjectId: "",
            mode: "READ",
            lockOperationState: "IDLE",
            timestamp: new Date().toISOString()
        }, TelemetryRuntime.workflowContextFromStateModel(oStateModel), oOptions.payload || {});

        return {
            event: String(sEventName || "telemetry.event"),
            correlationId: String(oPayload.correlationId || CorrelationId.next("tel")).trim(),
            timestamp: String(oPayload.timestamp || new Date().toISOString()),
            payload: oPayload
        };
    }

    function persist(oStateModel, oEvent) {
        var oBufferState = MemoryTelemetryBuffer.push(oEvent);
        ModelStateRuntime.setManyOnModel(oStateModel, {
            "/telemetry/events": oBufferState.events,
            "/telemetry/lastEvent": oBufferState.lastEvent
        });
        return oEvent;
    }

    function emit(sEventName, mOptions) {
        var oOptions = mOptions || {};
        var oEvent = buildEvent(sEventName, oOptions);
        DebugTelemetryAdapter.emit(oEvent);
        return persist(oOptions.stateModel, oEvent);
    }

    return {
        emit: emit
    };
});
