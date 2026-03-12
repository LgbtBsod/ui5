sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts"
], function (StatePaths, CreateSentinel, ModelStateRuntime, WorkflowContracts, ModelPathContracts) {
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

    function syncUiStateMode(oStateModel, oUiStateModel) {
        ModelStateRuntime.setManyOnModel(oUiStateModel, {
            "/busy": !!(
                ModelStateRuntime.readOnModel(oStateModel, StatePaths.UI_BUSY_DETAIL, false)
                || ModelStateRuntime.readOnModel(oStateModel, "/isLoading", false)
            ),
            "/currentRootKey": ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID || "/activeObjectId", "") || "",
            "/sessionGuid": ModelStateRuntime.readOnModel(oStateModel, StatePaths.SESSION_ID, "") || ""
        });
    }

    function syncDetailCurrentFromSelected() {
        return;
    }

    function resolveDetailCurrent(oSelectedModel) {
        var oSelected = oSelectedModel.getData() || {};
        if (oSelected && oSelected.root) {
            return ModelStateRuntime.clone(oSelected, {});
        }
        return ModelStateRuntime.clone(oSelected, {});
    }

    return {
        resolveBootDetailId: resolveBootDetailId,
        isCreateBootHash: isCreateBootHash,
        applyLockProbeState: applyLockProbeState,
        syncUiStateMode: syncUiStateMode,
        syncDetailCurrentFromSelected: syncDetailCurrentFromSelected,
        resolveDetailCurrent: resolveDetailCurrent
    };
});
