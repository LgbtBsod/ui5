sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts"
], function (StatePaths, ModelStateRuntime, WorkflowContracts) {
    "use strict";

    function formatHumanDateTime(vDate) {
        var oDate = vDate instanceof Date ? vDate : new Date(vDate || Date.now());
        if (Number.isNaN(oDate.getTime())) {
            oDate = new Date();
        }
        return oDate.toLocaleString(undefined, {
            year: "numeric",
            month: "short",
            day: "2-digit",
            hour: "2-digit",
            minute: "2-digit"
        });
    }

    function eventPayload(oEvent) {
        return (oEvent && typeof oEvent.getParameters === "function" && oEvent.getParameters()) || {};
    }

    function applyLockProbeState(oPayload, oStateModel) {
        var bKilled = !!(oPayload && (oPayload.killed || oPayload.is_killed));
        var bOk = !!(oPayload && (oPayload.ok || oPayload.lockOk || oPayload.success || oPayload.Ok));
        var bLost = !bKilled && !bOk &&
            ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "") === WorkflowContracts.EDIT_MODES.EDIT &&
            ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "") === WorkflowContracts.LOCK_STATES.EDIT_LOCKED;
        return {
            killed: bKilled,
            lost: bLost,
            lockExpires: (oPayload && (oPayload.lock_expires || oPayload.expiresAt)) || null,
            reason: (oPayload && (oPayload.code || oPayload.reason_code || oPayload.ReasonCode)) || (bKilled ? WorkflowContracts.REASONS.KILLED : (bLost ? WorkflowContracts.REASONS.LOST : WorkflowContracts.REASONS.OWNED_BY_YOU))
        };
    }

    return {
        formatHumanDateTime: formatHumanDateTime,
        eventPayload: eventPayload,
        applyLockProbeState: applyLockProbeState
    };
});
