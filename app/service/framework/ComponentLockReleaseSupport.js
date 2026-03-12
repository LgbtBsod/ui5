sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayBackendService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts"
], function (StatePaths, CreateSentinel, GatewayBackendService, RootIdRuntime, LayoutStateRuntime, ModelStateRuntime, WorkflowContracts) {
    "use strict";

    function encodeUrlParameters(mParameters) {
        return Object.keys(mParameters || {}).reduce(function (aPairs, sKey) {
            var vValue = mParameters[sKey];
            if (vValue === undefined || vValue === null || vValue === "") {
                return aPairs;
            }
            aPairs.push(encodeURIComponent(sKey) + "=" + encodeURIComponent(String(vValue)));
            return aPairs;
        }, []).join("&");
    }

    function readActiveLockPayload(oStateModel) {
        var sRootId = RootIdRuntime.resolveActiveFromStateModel(oStateModel);
        var sSessionGuid = String(ModelStateRuntime.readOnModel(oStateModel, StatePaths.SESSION_ID, "") || "").trim();
        var sMode = LayoutStateRuntime.normalizeMode(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, ""), "");
        var sLockState = LayoutStateRuntime.normalizeState(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, ""), "");
        if (!sRootId || !sSessionGuid || CreateSentinel.isCreateId(sRootId)) {
            return null;
        }
        if (sMode !== WorkflowContracts.EDIT_MODES.EDIT || sLockState !== WorkflowContracts.LOCK_STATES.EDIT_LOCKED) {
            return null;
        }
        return {
            RootId: sRootId,
            SessionGuid: sSessionGuid
        };
    }

    function buildLockReleaseUrl(oStateModel) {
        var sServiceUrl = String(ModelStateRuntime.readOnModel(oStateModel, "/backendServiceUrl", "") || "").trim() || GatewayBackendService.serviceUrl();
        if (!sServiceUrl) {
            return "";
        }
        return String(sServiceUrl).replace(/\/+$/, "") + "/LockRelease";
    }

    function tryBeaconLockRelease(sUrl, oPayload, sToken) {
        if (!sUrl || !oPayload || !navigator || typeof navigator.sendBeacon !== "function") {
            return false;
        }
        try {
            return navigator.sendBeacon(sUrl + "?" + encodeUrlParameters(Object.assign({}, oPayload, sToken ? { CsrfToken: sToken } : {})), new Blob([JSON.stringify(oPayload)], { type: "application/json" }));
        } catch (e) {
            return false;
        }
    }

    return {
        readActiveLockPayload: readActiveLockPayload,
        buildLockReleaseUrl: buildLockReleaseUrl,
        tryBeaconLockRelease: tryBeaconLockRelease
    };
});
