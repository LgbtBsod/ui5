sap.ui.define([
    "checklist/app/model/StatePaths",
    "checklist/app/util/CreateSentinel",
    "checklist/app/service/backend/GatewayBackendService"
], function (StatePaths, CreateSentinel, GatewayBackendService) {
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
        var sRootId = String(oStateModel && oStateModel.getProperty ? oStateModel.getProperty("/activeObjectId") || "" : "").trim();
        var sSessionGuid = String(oStateModel && oStateModel.getProperty ? oStateModel.getProperty(StatePaths.SESSION_ID) || "" : "").trim();
        var sMode = String(oStateModel && oStateModel.getProperty ? oStateModel.getProperty(StatePaths.WORKFLOW_EDIT_MODE) || "" : "").toUpperCase();
        var sLockState = String(oStateModel && oStateModel.getProperty ? oStateModel.getProperty(StatePaths.WORKFLOW_LOCK_STATUS) || "" : "").toUpperCase();
        var oCurrentUser = oStateModel && oStateModel.getProperty ? (oStateModel.getProperty("/currentUser") || {}) : {};
        var sUser = String(oCurrentUser.uname || "").trim();
        if (!sRootId || !sSessionGuid || CreateSentinel.isCreateId(sRootId)) {
            return null;
        }
        if (sMode !== "EDIT" || sLockState !== "LOCKED") {
            return null;
        }
        return {
            RootId: sRootId,
            SessionGuid: sSessionGuid,
            Uname: sUser
        };
    }

    function buildLockReleaseUrl(oStateModel) {
        var sServiceUrl = String(oStateModel && oStateModel.getProperty ? oStateModel.getProperty("/backendServiceUrl") || "" : "").trim() || GatewayBackendService.serviceUrl();
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
