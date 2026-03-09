sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime"
], function (StatePaths, CreateSentinel, ModelStateRuntime) {
    "use strict";

    function resolveBootDetailId(sHash) {
        var s = String(sHash || "").trim();
        var oMatch = /^#\/?checklist\/([^\/?#]+)(?:\/[^?#]+)?$/i.exec(s);
        return oMatch ? decodeURIComponent(oMatch[1] || "") : "";
    }

    function isCreateBootHash(sHash) {
        return CreateSentinel.isCreateId(resolveBootDetailId(sHash));
    }

    function ensureSessionId(oStateModel) {
        var sCurrent = ModelStateRuntime.readOnModel(oStateModel, StatePaths.SESSION_ID, "");
        var sStored = window.sessionStorage.getItem("pcct_session_id") || "";
        if (sCurrent) {
            return sCurrent;
        }
        if (sStored) {
            ModelStateRuntime.writeOnModel(oStateModel, StatePaths.SESSION_ID, sStored);
            return sStored;
        }
        var sNext = "S" + Math.random().toString(36).slice(2) + Date.now().toString(36);
        ModelStateRuntime.writeOnModel(oStateModel, StatePaths.SESSION_ID, sNext);
        window.sessionStorage.setItem("pcct_session_id", sNext);
        return sNext;
    }

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
        var bLost = !bKilled && !bOk && ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_EDIT_MODE, "") === "EDIT";
        return {
            killed: bKilled,
            lost: bLost,
            lockExpires: (oPayload && (oPayload.lock_expires || oPayload.expiresAt)) || null,
            reason: (oPayload && (oPayload.code || oPayload.reason_code || oPayload.ReasonCode)) || (bKilled ? "KILLED" : (bLost ? "LOST" : "OWNED_BY_YOU"))
        };
    }

    function syncUiStateMode(oStateModel, oUiStateModel) {
        ModelStateRuntime.setManyOnModel(oUiStateModel, {
            "/mode": ModelStateRuntime.readOnModel(oStateModel, "/mode", "READ") || "READ",
            "/busy": !!(ModelStateRuntime.readOnModel(oStateModel, "/isBusy", false) || ModelStateRuntime.readOnModel(oStateModel, "/isLoading", false)),
            "/currentRootKey": ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "") || "",
            "/sessionGuid": ModelStateRuntime.readOnModel(oStateModel, StatePaths.SESSION_ID, "") || ""
        });
    }

    function syncDetailCurrentFromSelected(oSelectedModel, oUiStateModel) {
        var oSelected = oSelectedModel.getData() || {};
        ModelStateRuntime.writeOnModel(oUiStateModel, "/_detailCurrent", ModelStateRuntime.clone(oSelected, {}));
    }

    function resolveDetailCurrent(oSelectedModel, oUiStateModel) {
        var oSelected = oSelectedModel.getData() || {};
        var oCurrent = ModelStateRuntime.readOnModel(oUiStateModel, "/_detailCurrent", {}) || {};
        if (oSelected && oSelected.root) {
            ModelStateRuntime.writeOnModel(oUiStateModel, "/_detailCurrent", ModelStateRuntime.clone(oSelected, {}));
            return ModelStateRuntime.readOnModel(oUiStateModel, "/_detailCurrent", {}) || {};
        }
        if (oCurrent && oCurrent.root) {
            return oCurrent;
        }
        syncDetailCurrentFromSelected(oSelectedModel, oUiStateModel);
        return ModelStateRuntime.readOnModel(oUiStateModel, "/_detailCurrent", {}) || {};
    }

    return {
        resolveBootDetailId: resolveBootDetailId,
        isCreateBootHash: isCreateBootHash,
        ensureSessionId: ensureSessionId,
        formatHumanDateTime: formatHumanDateTime,
        eventPayload: eventPayload,
        applyLockProbeState: applyLockProbeState,
        syncUiStateMode: syncUiStateMode,
        syncDetailCurrentFromSelected: syncDetailCurrentFromSelected,
        resolveDetailCurrent: resolveDetailCurrent
    };
});
