sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/GatewayRequestRuntime"
], function (GatewayRequestRuntime) {
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
        var oResult = GatewayRequestRuntime.unwrap(oRawResult) || {};
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
        return GatewayRequestRuntime.postFunction("LockAcquire", {
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
        return GatewayRequestRuntime.postFunction("LockHeartbeat", {
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
        return GatewayRequestRuntime.get("LockStatusSet('" + sRootId + "')", { SessionGuid: sToken }).then(function (oResult) {
            return normalizeResult(oResult, sToken);
        }).catch(function (oError) {
            return { ok: false, code: "ERROR", killed: false, messageKey: "lockStatusFailed", raw: oError || {} };
        });
    }

    function release(mArgs) {
        var sRootId = String((mArgs && mArgs.rootId) || "").trim();
        var sToken = normalizeLockToken(mArgs);
        return GatewayRequestRuntime.postFunction("LockRelease", { RootId: sRootId, SessionGuid: sToken }).then(function (oResult) {
            var oNormalized = normalizeResult(oResult, sToken);
            return { ok: !!oNormalized.ok, code: oNormalized.code || "OK", released: true, killed: !!oNormalized.killed, messageKey: oNormalized.messageKey || "" };
        }).catch(function (oError) {
            if (isMissingLockRelease(oError)) {
                return { ok: true, code: "OK", released: false, killed: false, messageKey: "" };
            }
            return { ok: false, code: "ERROR", released: false, killed: false, messageKey: "lockReleaseFailed" };
        });
    }

    return { acquire: acquire, heartbeat: heartbeat, status: status, release: release };
});
