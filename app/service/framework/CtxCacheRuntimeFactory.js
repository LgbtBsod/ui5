sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/usecases/CacheValidationUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/usecases/CacheReadUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/usecases/CacheWriteUseCase"
], function (
    CacheValidationUseCase,
    CacheReadUseCase,
    CacheWriteUseCase
) {
    "use strict";

    function buildCacheRuntime() {
        return {
            cacheValidation: CacheValidationUseCase(),
            cacheRead: CacheReadUseCase(),
            cacheWrite: CacheWriteUseCase()
        };
    }

    return {
        buildCacheRuntime: buildCacheRuntime
    };
});
