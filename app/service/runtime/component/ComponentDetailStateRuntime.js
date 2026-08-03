sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (StatePaths, CreateSentinel, ModelStateRuntime, CloneUtil, WorkflowContracts, ModelContracts) {
    "use strict";

    function resolveBootDetailId(sHash) {
        var s = String(sHash || "").trim();
        var oMatch = /^#\/?checklist\/([^\/?#]+)(?:\/[^?#]+)?$/i.exec(s);
        return oMatch ? decodeURIComponent(oMatch[1] || "") : "";
    }

    function isCreateBootHash(sHash) {
        return CreateSentinel.isCreateId(resolveBootDetailId(sHash));
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

    function resolveDetailCurrent(oDetailModel) {
        var oCurrent = ModelStateRuntime.readOnModel(oDetailModel, "/current", {}) || {};
        return CloneUtil.clone(oCurrent, {});
    }

    return {
        resolveBootDetailId: resolveBootDetailId,
        isCreateBootHash: isCreateBootHash,
        applyLockProbeState: applyLockProbeState,
        resolveDetailCurrent: resolveDetailCurrent
    };
});
