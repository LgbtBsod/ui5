sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/ports/BrowserCachePort"
], function (UseCase, Result, BrowserCachePort) {
    "use strict";

    function CacheWriteUseCase() { UseCase.call(this, "CacheWriteUseCase"); }
    CacheWriteUseCase.prototype = Object.create(UseCase.prototype);
    CacheWriteUseCase.prototype.constructor = CacheWriteUseCase;

    CacheWriteUseCase.prototype.execute = function (mInput, mCtx) {
        var sRootId = (mInput && mInput.rootId) || "";
        var oSnapshot = (mInput && mInput.snapshot) || null;
        var oCache = mCtx && mCtx.cache;
        var bPortLike = !!(BrowserCachePort && BrowserCachePort.prototype && typeof oCache.write === "function");
        if (!sRootId || !oCache || !bPortLike) return Promise.resolve(Result.ok({ written: false }));
        return Promise.resolve(oCache.write(sRootId, oSnapshot)).then(function () { return Result.ok({ written: true }); });
    };

    return CacheWriteUseCase;
});
