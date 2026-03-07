sap.ui.define([
    "sap_ui5/infra/adapters/shared/GatewayAdapterSupport",
    "sap_ui5/infra/adapters/shared/GatewayIdentitySupport"
], function (GatewayAdapterSupport, GatewayIdentitySupport) {
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

    function normalizeResult(oRawResult, sToken) {
        var oResult = GatewayAdapterSupport.unwrap(oRawResult) || {};
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

    function create(mDeps) {
        function withUserName(oPayload) {
            return GatewayIdentitySupport.withUserName(oPayload, mDeps || {});
        }

        return {
            acquire: function (mArgs) {
                var sRootId = mArgs && mArgs.rootId;
                var sSession = normalizeLockToken(mArgs);
                return GatewayAdapterSupport.postFunction("LockAcquire", withUserName({ RootId: sRootId, SessionGuid: sSession, Force: !!(mArgs && mArgs.force), StealFrom: (mArgs && mArgs.stealFrom) || (mArgs && mArgs.force ? sSession : "") })).then(function (oResult) {
                    return normalizeResult(oResult, sSession);
                }).catch(function (oError) {
                    return { ok: false, code: "ERROR", killed: false, messageKey: "lockAcquireFailed", raw: oError || {} };
                });
            },
            heartbeat: function (mArgs) {
                var sRootId = mArgs && mArgs.rootId;
                var sToken = normalizeLockToken(mArgs);
                return GatewayAdapterSupport.postFunction("LockHeartbeat", withUserName({ RootId: sRootId, SessionGuid: sToken })).then(function (oResult) {
                    return normalizeResult(oResult, sToken);
                }).catch(function (oError) {
                    return { ok: false, code: "ERROR", killed: false, messageKey: "lockHeartbeatFailed", raw: oError || {} };
                });
            },
            status: function (mArgs) {
                var sRootId = mArgs && mArgs.rootId;
                var sToken = normalizeLockToken(mArgs);
                return GatewayAdapterSupport.get("LockStatusSet('" + String(sRootId || "").trim() + "')", { SessionGuid: sToken }).then(function (oResult) {
                    return normalizeResult(oResult, sToken);
                }).catch(function (oError) {
                    return { ok: false, code: "ERROR", killed: false, messageKey: "lockStatusFailed", raw: oError || {} };
                });
            },
            release: function (mArgs) {
                var sRootId = mArgs && mArgs.rootId;
                var sToken = normalizeLockToken(mArgs);
                return GatewayAdapterSupport.postFunction("LockRelease", withUserName({ RootId: sRootId, SessionGuid: sToken })).then(function (oResult) {
                    var oNormalized = normalizeResult(oResult, sToken);
                    return { ok: !!oNormalized.ok, code: oNormalized.code || "OK", released: true, killed: !!oNormalized.killed, messageKey: oNormalized.messageKey || "" };
                }).catch(function () {
                    return { ok: false, code: "ERROR", released: false, killed: false, messageKey: "lockReleaseFailed" };
                });
            }
        };
    }

    return { create: create };
});
