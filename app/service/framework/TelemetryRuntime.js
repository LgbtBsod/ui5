sap.ui.define([], function () {
    "use strict";

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

    return {
        stateValue: stateValue,
        objectRef: objectRef,
        lockLost: lockLost,
        saveFailure: saveFailure,
        runtimeConfig: runtimeConfig
    };
});
