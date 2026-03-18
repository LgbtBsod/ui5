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

    function buildRequestUrl(sUrl, oPayload) {
        var aQueryParts = [];
        Object.keys(oPayload || {}).forEach(function (sKey) {
            var vValue = oPayload[sKey];
            if (vValue === null || typeof vValue === "undefined" || String(vValue).trim() === "") {
                return;
            }
            aQueryParts.push(encodeURIComponent(sKey) + "=" + encodeURIComponent(String(vValue)));
        });
        if (!sUrl || !aQueryParts.length) {
            return "";
        }
        return sUrl + (sUrl.indexOf("?") >= 0 ? "&" : "?") + aQueryParts.join("&");
    }

    function isFetchResponseSuccessful(oResponse) {
        if (!oResponse) {
            return false;
        }
        if (typeof oResponse.ok === "boolean") {
            return oResponse.ok;
        }
        return true;
    }

    function tryBeaconLockRelease(sUrl, oPayload, sToken) {
        var sRequestUrl = buildRequestUrl(sUrl, oPayload);
        var pRelease;
        if (!sRequestUrl || !sToken || typeof window === "undefined" || typeof window.fetch !== "function") {
            return null;
        }
        try {
            pRelease = Promise.resolve(window.fetch(sRequestUrl, {
                method: "POST",
                headers: {
                    "Accept": "application/json",
                    "X-CSRF-Token": sToken
                },
                credentials: "same-origin",
                keepalive: true
            })).then(isFetchResponseSuccessful).catch(function () {
                return false;
            });
            return {
                queued: true,
                promise: pRelease
            };
        } catch (_fetchError) {
            return null;
        }
    }

    return {
        readActiveLockPayload: readActiveLockPayload,
        buildLockReleaseUrl: buildLockReleaseUrl,
        tryBeaconLockRelease: tryBeaconLockRelease
    };
});
