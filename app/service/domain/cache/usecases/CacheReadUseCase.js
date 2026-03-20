sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/ports/BrowserCachePort"
], function (Result, BrowserCachePort) {
    "use strict";

    function CacheReadUseCase() {
        return {
            execute: execute
        };
    }

function execute(mInput, mCtx) {
        var sRootId = (mInput && mInput.rootId) || "";
        var sEntityKind = (mInput && mInput.entityKind) || "detailSnapshot";
        var oCache = mCtx && mCtx.cache;
        var bPortLike = !!(BrowserCachePort && BrowserCachePort.prototype && typeof oCache.read === "function");
        if (!sRootId || !oCache || !bPortLike) return Promise.resolve(Result.ok({ snapshot: null }));
        return Promise.resolve(oCache.read(sRootId, sEntityKind)).then(function (oEntry) {
            return Result.ok({
                entry: oEntry || null,
                snapshot: (oEntry && oEntry.payload) || null
            });
        });
    }

    return CacheReadUseCase;
});