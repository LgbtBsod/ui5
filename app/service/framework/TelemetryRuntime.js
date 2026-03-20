sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts"
], function (ModelStateRuntime, StatePaths, WorkflowContracts, ModelPathContracts) {
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
        return objectRef(stateRead(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, ""));
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

    function runtimeConfig(vSource, vError, oOriginalError) {
        var mPayload = {
            source: String(vSource || "")
        };
        var sCode = String((oOriginalError && oOriginalError.code) || "").trim();
        var iStatus = Number((oOriginalError && (oOriginalError.statusCode || oOriginalError.status)) || 0) || 0;
        var sReason = String((oOriginalError && oOriginalError.message) || "").trim();

        if (typeof vError !== "undefined" && vError !== null && vError !== "") {
            mPayload.error = String(vError);
        }
        if (sCode) {
            mPayload.errorCode = sCode;
        }
        if (iStatus > 0) {
            mPayload.statusCode = iStatus;
        }
        if (sReason) {
            mPayload.reason = sReason;
        }
        return mPayload;
    }

    function workflowContextFromStateModel(oStateModel) {
        return {
            sessionId: String(stateRead(oStateModel, "/sessionId", "")),
            activeObjectId: String(stateRead(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, "")),
            mode: String(stateRead(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ)),
            lockState: String(stateRead(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY))
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
