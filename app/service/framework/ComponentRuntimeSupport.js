sap.ui.define([
    "checklist/app/model/StatePaths",
    "checklist/app/util/CreateSentinel",
    "checklist/app/service/framework/ModelStateRuntime"
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
        var sCurrent = oStateModel.getProperty(StatePaths.SESSION_ID);
        var sStored = window.sessionStorage.getItem("pcct_session_id") || "";
        if (sCurrent) {
            return sCurrent;
        }
        if (sStored) {
            oStateModel.setProperty(StatePaths.SESSION_ID, sStored);
            return sStored;
        }
        var sNext = "S" + Math.random().toString(36).slice(2) + Date.now().toString(36);
        oStateModel.setProperty(StatePaths.SESSION_ID, sNext);
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
        var bLost = !bKilled && !bOk && oStateModel.getProperty(StatePaths.WORKFLOW_EDIT_MODE) === "EDIT";
        return {
            killed: bKilled,
            lost: bLost,
            lockExpires: (oPayload && (oPayload.lock_expires || oPayload.expiresAt)) || null,
            reason: (oPayload && (oPayload.code || oPayload.reason_code || oPayload.ReasonCode)) || (bKilled ? "KILLED" : (bLost ? "LOST" : "OWNED_BY_YOU"))
        };
    }

    function syncUiStateMode(oStateModel, oUiStateModel) {
        oUiStateModel.setProperty("/mode", oStateModel.getProperty("/mode") || "READ");
        oUiStateModel.setProperty("/busy", !!(oStateModel.getProperty("/isBusy") || oStateModel.getProperty("/isLoading")));
        oUiStateModel.setProperty("/currentRootKey", oStateModel.getProperty("/activeObjectId") || "");
        oUiStateModel.setProperty("/sessionGuid", oStateModel.getProperty(StatePaths.SESSION_ID) || "");
    }

    function syncDetailCurrentFromSelected(oSelectedModel, oUiStateModel) {
        var oSelected = oSelectedModel.getData() || {};
        oUiStateModel.setProperty("/_detailCurrent", ModelStateRuntime.clone(oSelected, {}));
    }

    function resolveDetailCurrent(oSelectedModel, oUiStateModel) {
        var oSelected = oSelectedModel.getData() || {};
        var oCurrent = oUiStateModel.getProperty("/_detailCurrent") || {};
        if (oSelected && oSelected.root) {
            oUiStateModel.setProperty("/_detailCurrent", ModelStateRuntime.clone(oSelected, {}));
            return oUiStateModel.getProperty("/_detailCurrent") || {};
        }
        if (oCurrent && oCurrent.root) {
            return oCurrent;
        }
        syncDetailCurrentFromSelected(oSelectedModel, oUiStateModel);
        return oUiStateModel.getProperty("/_detailCurrent") || {};
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
