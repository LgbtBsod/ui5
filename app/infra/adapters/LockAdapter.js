sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/odata/GatewayODataClient",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient"
], function (GatewayODataClient, ODataAdapterUtils, GatewayContractConstants, GatewayClient) {
    "use strict";

    function normalizeLockToken(mArgs) {
        return (mArgs && (mArgs.sessionGuid || mArgs.lockToken)) || "";
    }

    function resolveOwner(oResult) {
        var sOwnerUser = String((oResult && (oResult.owner || oResult.Owner || oResult.lock_owner_user || "")) || "").trim();
        var sOwnerSession = String((oResult && (
            oResult.owner_session ||
            oResult.OwnerSession ||
            oResult.owner_session_guid ||
            oResult.OwnerSessionGuid ||
            oResult.lock_owner_session ||
            ""
        )) || "").trim();
        return { userId: sOwnerUser, sessionGuid: sOwnerSession };
    }

    function resolveCode(oResult, bOk, bKilled, bCanTakeover) {
        if (bKilled) { return "KILLED"; }
        if (bOk) { return "OK"; }
        if (bCanTakeover) { return "LOCKED_OWN_SESSION"; }
        if (oResult && (oResult.expired || oResult.is_expired || oResult.code === "EXPIRED")) { return "EXPIRED"; }
        return "LOCKED";
    }

    function isMissingLockRelease(oError) {
        var iStatus = Number((oError && (oError.statusCode || oError.status)) || 0) || 0;
        var sCode = String((oError && oError.code) || "").trim().toUpperCase();
        return iStatus === 404 || sCode === "404" || sCode === "NOT_FOUND";
    }

    function normalizeResult(oRawResult, sToken) {
        var oResult = ODataAdapterUtils.unwrap(oRawResult) || {};
        var bOk = !!(oResult.success || oResult.Success || oResult.Ok || oResult.lockOk || oResult.ok);
        var bKilled = !!(oResult.is_killed || oResult.IsKilled || oResult.killed);
        var bCanTakeover = !!(oResult.can_takeover || oResult.CanTakeover || oResult.locked_by_same_user);
        var oOwner = resolveOwner(oResult);
        var sCode = resolveCode(oResult, bOk, bKilled, bCanTakeover);
        var m = {
            ok: bOk,
            code: sCode,
            lockOk: bOk,
            lockToken: sToken,
            expiresAt: (oResult.LockExpires || oResult.lock_expires || oResult.lock_expires_on || oResult.ExpiresOn || oResult.expiresAt) || "",
            killed: bKilled,
            owner: oOwner,
            canTakeover: bCanTakeover,
            messageKey: bKilled ? "lockKilledMessage" : (sCode === "LOCKED" ? "lockAcquireFailed" : ""),
            raw: oResult
        };
        m.ownerName = oOwner.userId;
        return m;
    }

    function acquire(mArgs) {
        var sRootId = String((mArgs && mArgs.rootId) || "").trim();
        var sSession = normalizeLockToken(mArgs);
        var sObjectUuid = String((mArgs && (mArgs.objectUuid || mArgs.ObjectUuid)) || sRootId).trim();
        var sTabSessionId = String((mArgs && (mArgs.tabSessionId || mArgs.TabSessionId)) || "").trim();
        return GatewayODataClient.postFunction(GatewayContractConstants.FUNCTION_IMPORTS.LOCK_ACQUIRE, {
            ObjectUuid: sObjectUuid,
            RootId: sRootId,
            SessionGuid: sSession,
            TabSessionId: sTabSessionId,
            Force: !!(mArgs && mArgs.force),
            StealFrom: (mArgs && mArgs.stealFrom) || (mArgs && mArgs.force ? sSession : "")
        }).then(function (oResult) {
            return normalizeResult(oResult, sSession);
        }).catch(function (oError) {
            return { ok: false, code: "ERROR", killed: false, messageKey: "lockAcquireFailed", raw: oError || {} };
        });
    }

    function heartbeat(mArgs) {
        var sRootId = String((mArgs && mArgs.rootId) || "").trim();
        var sToken = normalizeLockToken(mArgs);
        return GatewayODataClient.postFunction(GatewayContractConstants.FUNCTION_IMPORTS.LOCK_HEARTBEAT, {
            ObjectUuid: String((mArgs && (mArgs.objectUuid || mArgs.ObjectUuid)) || sRootId).trim(),
            RootId: sRootId,
            SessionGuid: sToken
        }).then(function (oResult) {
            return normalizeResult(oResult, sToken);
        }).catch(function (oError) {
            return { ok: false, code: "ERROR", killed: false, messageKey: "lockHeartbeatFailed", raw: oError || {} };
        });
    }

    function status(mArgs) {
        var sRootId = String((mArgs && mArgs.rootId) || "").trim();
        var sToken = normalizeLockToken(mArgs);
        return GatewayODataClient.get(GatewayContractConstants.ENTITY_SETS.LOCK_STATUS + "('" + sRootId + "')", { SessionGuid: sToken }).then(function (oResult) {
            return normalizeResult(oResult, sToken);
        }).catch(function (oError) {
            return { ok: false, code: "ERROR", killed: false, messageKey: "lockStatusFailed", raw: oError || {} };
        });
    }

    function release(mArgs) {
        var sRootId = String((mArgs && mArgs.rootId) || "").trim();
        var sToken = normalizeLockToken(mArgs);
        return GatewayODataClient.postFunction(GatewayContractConstants.FUNCTION_IMPORTS.LOCK_RELEASE, {
            RootId: sRootId,
            ObjectUuid: String((mArgs && (mArgs.objectUuid || mArgs.ObjectUuid)) || sRootId).trim(),
            SessionGuid: sToken
        }).then(function (oResult) {
            var oNormalized = normalizeResult(oResult, sToken);
            return { ok: !!oNormalized.ok, code: oNormalized.code || "OK", released: true, killed: !!oNormalized.killed, messageKey: oNormalized.messageKey || "" };
        }).catch(function (oError) {
            if (isMissingLockRelease(oError)) {
                return { ok: true, code: "OK", released: false, killed: false, messageKey: "" };
            }
            return { ok: false, code: "ERROR", released: false, killed: false, messageKey: "lockReleaseFailed" };
        });
    }

    function releaseOnPageLeave(mArgs) {
        var sRootId = String((mArgs && mArgs.rootId) || "").trim();
        var sToken = normalizeLockToken(mArgs);
        var oModel;
        var sServiceUrl;
        var sCsrfToken;
        var oPayload;
        if (!sRootId || !sToken || typeof window === "undefined" || typeof window.fetch !== "function") {
            return false;
        }
        try {
            oModel = GatewayClient.getModel();
            sServiceUrl = GatewayClient.serviceUrl();
            sCsrfToken = String((oModel && oModel.getSecurityToken && oModel.getSecurityToken()) || "").trim();
        } catch (_error) {
            return false;
        }
        if (!sServiceUrl || !sCsrfToken) {
            return false;
        }
        oPayload = {
            RootId: sRootId,
            ObjectUuid: String((mArgs && (mArgs.objectUuid || mArgs.ObjectUuid)) || sRootId).trim(),
            SessionGuid: sToken
        };
        try {
            window.fetch(String(sServiceUrl).replace(/\/+$/, "") + "/" + GatewayContractConstants.FUNCTION_IMPORTS.LOCK_RELEASE, {
                method: "POST",
                keepalive: true,
                credentials: "same-origin",
                headers: {
                    "Content-Type": "application/json",
                    "Accept": "application/json",
                    "X-CSRF-Token": sCsrfToken
                },
                body: JSON.stringify(oPayload)
            });
            return true;
        } catch (_fetchError) {
            return false;
        }
    }

    return { acquire: acquire, heartbeat: heartbeat, status: status, release: release, releaseOnPageLeave: releaseOnPageLeave };
});
