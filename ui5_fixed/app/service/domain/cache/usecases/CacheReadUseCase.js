sap.ui.define([
    "checklist/app/service/framework/UseCase",
    "checklist/app/service/framework/Result",
    "checklist/app/service/domain/cache/ports/BrowserCachePort"
], function (UseCase, Result, BrowserCachePort) {
    "use strict";

    function CacheReadUseCase() { UseCase.call(this, "CacheReadUseCase"); }
    CacheReadUseCase.prototype = Object.create(UseCase.prototype);
    CacheReadUseCase.prototype.constructor = CacheReadUseCase;

    CacheReadUseCase.prototype.execute = function (mInput, mCtx) {
        var sRootId = (mInput && mInput.rootId) || "";
        var oCache = mCtx && mCtx.cache;
        var bPortLike = !!(BrowserCachePort && BrowserCachePort.prototype && typeof oCache.read === "function");
        if (!sRootId || !oCache || !bPortLike) return Promise.resolve(Result.ok({ snapshot: null }));
        return Promise.resolve(oCache.read(sRootId)).then(function (oData) { return Result.ok({ snapshot: oData || null }); });
    };

    return CacheReadUseCase;
});
