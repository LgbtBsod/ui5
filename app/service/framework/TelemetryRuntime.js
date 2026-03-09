sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime"
], function (ModelStateRuntime) {
    "use strict";

    function stateRead(oStateModel, sPath, vFallback) {
        var vValue;
        vValue = ModelStateRuntime.readOnModel(oStateModel, sPath, vFallback);
        return typeof vValue === "undefined" ? vFallback : vValue;
    }

    function stateValue(vValue) {
        return {
            value: vValue
        };
    }

    function objectRef(vObjectId) {
        return {
            objectId: vObjectId || null
        };
    }

    function objectRefFromStateModel(oStateModel) {
        return objectRef(stateRead(oStateModel, "/activeObjectId", ""));
    }

    function lockLost(vReason, vSource) {
        return {
            reason: String(vReason || ""),
            source: String(vSource || "")
        };
    }

    function saveFailure(sRootId, oError, sCorrelationId) {
        return {
            rootId: String(sRootId || ""),
            code: String((oError && oError.code) || ""),
            statusCode: Number((oError && oError.statusCode) || 0) || 0,
            correlationId: String(sCorrelationId || "")
        };
    }

    function runtimeConfig(vSource, vError) {
        var mPayload = {
            source: String(vSource || "")
        };
        if (typeof vError !== "undefined" && vError !== null && vError !== "") {
            mPayload.error = String(vError);
        }
        return mPayload;
    }

    function workflowContextFromStateModel(oStateModel) {
        return {
            sessionId: String(stateRead(oStateModel, "/sessionId", "")),
            activeObjectId: String(stateRead(oStateModel, "/activeObjectId", "")),
            mode: String(stateRead(oStateModel, "/mode", "READ")),
            lockOperationState: String(stateRead(oStateModel, "/lockOperationState", "IDLE"))
        };
    }

    return {
        stateRead: stateRead,
        stateValue: stateValue,
        objectRef: objectRef,
        objectRefFromStateModel: objectRefFromStateModel,
        lockLost: lockLost,
        saveFailure: saveFailure,
        runtimeConfig: runtimeConfig,
        workflowContextFromStateModel: workflowContextFromStateModel
    };
});
