sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/ports/BrowserCachePort"
], function (Result, BrowserCachePort) {
    "use strict";

    function readSnapshotStamp(oSnap) {
        var oMeta = oSnap && oSnap.meta;
        var oRoot = oSnap && oSnap.root;
        return String(
            (oMeta && oMeta.aggChangedOn)
            || (oRoot && oRoot.server_changed_on)
            || (oSnap && oSnap._aggChangedOn)
            || ""
        ).trim();
    }

    function CacheWriteUseCase() {
        return {
            execute: execute
        };
    }

function execute(mInput, mCtx) {
        var sRootId = (mInput && mInput.rootId) || "";
        var oSnapshot = (mInput && mInput.snapshot) || null;
        var sEntityKind = (mInput && mInput.entityKind) || "detailSnapshot";
        var sLastChangeSet = String((mInput && mInput.lastChangeSet) || readSnapshotStamp(oSnapshot) || "").trim();
        var oCache = mCtx && mCtx.cache;
        var bPortLike = !!(BrowserCachePort && BrowserCachePort.prototype && typeof oCache.write === "function");
        if (!sRootId || !oCache || !bPortLike) return Promise.resolve(Result.ok({ written: false }));
        return Promise.resolve(oCache.write(sRootId, oSnapshot, {
            entityKind: sEntityKind,
            lastChangeSet: sLastChangeSet,
            validatedAt: new Date().toISOString()
        })).then(function (oEntry) {
            return Result.ok({
                entry: oEntry || null,
                written: true
            });
        });
    }

    return CacheWriteUseCase;
});