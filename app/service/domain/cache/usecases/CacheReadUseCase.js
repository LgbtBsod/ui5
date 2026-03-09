sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/ports/BrowserCachePort"
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
