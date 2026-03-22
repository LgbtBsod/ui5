sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/MemoryTelemetryBuffer",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/DebugTelemetryAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts"
], function (ModelStateRuntime, TelemetryRuntime, MemoryTelemetryBuffer, DebugTelemetryAdapter, WorkflowContracts) {
    "use strict";

    function nextCorrelationId() {
        return [
            "tel",
            Date.now().toString(36),
            Math.random().toString(36).slice(2, 10)
        ].join("-");
    }

    function buildEvent(sEventName, mOptions) {
        var oOptions = mOptions || {};
        var oStateModel = oOptions.stateModel;
        var oPayload = Object.assign({
            sessionId: "",
            activeObjectId: "",
            mode: WorkflowContracts.EDIT_MODES.READ,
            lockState: WorkflowContracts.LOCK_STATES.IDLE,
            timestamp: new Date().toISOString()
        }, TelemetryRuntime.workflowContextFromStateModel(oStateModel), oOptions.payload || {});

        return {
            event: String(sEventName || "telemetry.event"),
            correlationId: String(oPayload.correlationId || nextCorrelationId()).trim(),
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
