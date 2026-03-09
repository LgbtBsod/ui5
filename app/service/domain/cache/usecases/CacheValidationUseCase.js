sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/ports/BrowserCachePort",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/ports/LastChangeSetPort",
    "PRODUCTION_CONTROL_CHECKLIST/util/WorkflowTelemetry"
], function (UseCase, Result, BrowserCachePort, LastChangeSetPort, WorkflowTelemetry) {
    "use strict";

    function toMs(v) {
        var s = String(v || "");
        if (/^\/Date\(/.test(s)) return Number(s.slice(6).split(")/")[0].split("+")[0].split("-")[0]) || 0;
        var n = Date.parse(s);
        return Number.isFinite(n) ? n : 0;
    }

    function readSnapshotStamp(oSnap) {
        var oMeta = oSnap && oSnap.meta;
        var oRoot = oSnap && oSnap.root;
        return (oMeta && oMeta.aggChangedOn)
            || (oRoot && oRoot.server_changed_on)
            || (oSnap && oSnap._aggChangedOn)
            || "";
    }

    function CacheValidationUseCase() { UseCase.call(this, "CacheValidationUseCase"); }
    CacheValidationUseCase.prototype = Object.create(UseCase.prototype);
    CacheValidationUseCase.prototype.constructor = CacheValidationUseCase;

    function emit(mCtx, oPayload) {
        WorkflowTelemetry.emit("cache.validation.result", {
            stateModel: mCtx && mCtx.stateModel,
            payload: oPayload || {}
        });
    }

    CacheValidationUseCase.prototype.execute = function (mInput, mCtx) {
        var sRootId = (mInput && mInput.rootId) || "";
        var iTolerance = Number((mInput && mInput.toleranceMs) || 5500);
        var oCache = mCtx && mCtx.cache;
        var oLastChangeSet = mCtx && mCtx.lastChangeSet;
        var bCachePortLike = !!(BrowserCachePort && BrowserCachePort.prototype && typeof oCache.read === "function");
        var bLcsPortLike = !!(LastChangeSetPort && LastChangeSetPort.prototype && typeof oLastChangeSet.readAggChangedOn === "function");
        if (!sRootId || !oCache || !oLastChangeSet || !bCachePortLike || !bLcsPortLike) {
            emit(mCtx, { rootId: sRootId, valid: false, reason: "cache_ports_missing" });
            return Promise.resolve(Result.ok({ valid: false, reason: "cache_ports_missing" }));
        }
        return Promise.all([
            Promise.resolve(oCache.read(sRootId)),
            Promise.resolve(oLastChangeSet.readAggChangedOn(sRootId))
        ]).then(function (a) {
            var oSnap = a[0] || null;
            var iSrv = Number(a[1] || 0);
            var iCli = toMs(readSnapshotStamp(oSnap));
            var bHasSnapshot = !!oSnap;
            var bValid = bHasSnapshot && Math.abs(iSrv - iCli) <= iTolerance;
            var bInvalidate = bHasSnapshot && !bValid;
            var pInvalidate = (bInvalidate && typeof oCache.clear === "function") ? Promise.resolve(oCache.clear(sRootId)).catch(function () { return null; }) : Promise.resolve(null);
            return pInvalidate.then(function () {
                emit(mCtx, { rootId: sRootId, valid: bValid, invalidated: bInvalidate, serverStamp: iSrv, cacheStamp: iCli });
                return Result.ok({ valid: bValid, invalidated: bInvalidate, serverStamp: iSrv, cacheStamp: iCli, snapshot: bValid ? oSnap : null });
            });
        });
    };

    return CacheValidationUseCase;
});
