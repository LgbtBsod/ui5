sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/ports/BrowserCachePort",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/ports/LastChangeSetPort",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowTelemetry",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel"
], function (Result, BrowserCachePort, LastChangeSetPort, WorkflowTelemetry, CreateSentinel) {
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

    function stampDeltaMs(iServerStamp, iCacheStamp) {
        return Math.abs(Number(iServerStamp || 0) - Number(iCacheStamp || 0));
    }

    function hasMeaningfulDetailData(oSnap) {
        var oRoot = oSnap && oSnap.root || {};
        var oBasic = oSnap && oSnap.basic || {};
        var aChecks = Array.isArray(oSnap && oSnap.checks) ? oSnap.checks : [];
        var aBarriers = Array.isArray(oSnap && oSnap.barriers) ? oSnap.barriers : [];
        var oMeta = oSnap && oSnap.meta || {};
        var bHasRootId = !!String(oRoot.id || oRoot.Key || oRoot.RootKey || "").trim();
        var bCreateDraft = CreateSentinel.isCreateId(oRoot.id);
        var bHasBasicPayload = Object.keys(oBasic).some(function (sKey) {
            return !!String(oBasic[sKey] || "").trim();
        });
        var bHasMetaStamp = !!String(oMeta.aggChangedOn || oSnap && oSnap._aggChangedOn || "").trim();
        var iExpectedRows = Number(oRoot.ChecksTotal || oRoot.checks_total || 0) + Number(oRoot.BarriersTotal || oRoot.barriers_total || 0);

        if (bHasMetaStamp && !bHasRootId && !bHasBasicPayload && !aChecks.length && !aBarriers.length) {
            return true;
        }
        if (!bHasRootId) {
            return false;
        }
        if (bCreateDraft) {
            return true;
        }
        if (!bHasBasicPayload) {
            return false;
        }
        if (iExpectedRows > 0 && !aChecks.length && !aBarriers.length) {
            return false;
        }
        return true;
    }

    function CacheValidationUseCase() {
        return {
            execute: execute
        };
    }

function emit(mCtx, oPayload) {
        WorkflowTelemetry.emit("cache.validation", {
            stateModel: mCtx && mCtx.stateModel,
            payload: oPayload || {}
        });
    }

    function execute(mInput, mCtx) {
        var sRootId = (mInput && mInput.rootId) || "";
        var sEntityKind = (mInput && mInput.entityKind) || "detailSnapshot";
        var iTolerance = Number((mInput && mInput.toleranceMs) || 5500);
        var oCache = mCtx && mCtx.cache;
        var oLastChangeSet = mCtx && mCtx.lastChangeSet;
        var bCachePortLike = !!(oCache && typeof oCache.read === "function");
        var bLcsPortLike = !!(oLastChangeSet && typeof oLastChangeSet.readAggChangedOn === "function");
        if (!sRootId || !oCache || !oLastChangeSet || !bCachePortLike || !bLcsPortLike) {
            emit(mCtx, { rootId: sRootId, valid: false, reason: "cache_ports_missing" });
            return Promise.resolve(Result.ok({ valid: false, reason: "cache_ports_missing" }));
        }
        return Promise.all([
            Promise.resolve(oCache.read(sRootId, sEntityKind)),
            Promise.resolve(oLastChangeSet.readAggChangedOn(sRootId))
        ]).then(function (a) {
            var oEntry = a[0] || null;
            var oSnap = oEntry && oEntry.payload || null;
            var iSrv = Number(a[1] || 0);
            var iCli = toMs((oEntry && oEntry.lastChangeSet) || readSnapshotStamp(oSnap));
            var bHasSnapshot = !!oSnap;
            var bHydratedSnapshot = bHasSnapshot && hasMeaningfulDetailData(oSnap);
            var iStampDeltaMs = stampDeltaMs(iSrv, iCli);
            var bValid = bHydratedSnapshot && iStampDeltaMs <= iTolerance;
            var bInvalidate = bHasSnapshot && !bValid;
            var pInvalidate = (bInvalidate && typeof oCache.clear === "function")
                ? Promise.resolve(oCache.clear(sRootId, sEntityKind)).catch(function () { return null; })
                : Promise.resolve(null);
            return pInvalidate.then(function () {
                if (!bHasSnapshot) {
                    WorkflowTelemetry.emit("cache.miss", {
                        stateModel: mCtx && mCtx.stateModel,
                        payload: { rootId: sRootId, entityKind: sEntityKind }
                    });
                } else if (bValid) {
                    WorkflowTelemetry.emit("cache.hit", {
                        stateModel: mCtx && mCtx.stateModel,
                        payload: { rootId: sRootId, entityKind: sEntityKind }
                    });
                } else if (bInvalidate) {
                    WorkflowTelemetry.emit("cache.invalidated", {
                        stateModel: mCtx && mCtx.stateModel,
                        payload: { rootId: sRootId, entityKind: sEntityKind }
                    });
                }
                emit(mCtx, {
                    rootId: sRootId,
                    valid: bValid,
                    invalidated: bInvalidate,
                    hydrated: bHydratedSnapshot,
                    serverStamp: iSrv,
                    cacheStamp: iCli,
                    stampDeltaMs: iStampDeltaMs,
                    toleranceMs: iTolerance
                });
                return Result.ok({
                    cacheEntry: oEntry,
                    valid: bValid,
                    invalidated: bInvalidate,
                    hydrated: bHydratedSnapshot,
                    serverStamp: iSrv,
                    cacheStamp: iCli,
                    stampDeltaMs: iStampDeltaMs,
                    snapshot: bValid ? oSnap : null
                });
            });
        });
    }

    return CacheValidationUseCase;
});