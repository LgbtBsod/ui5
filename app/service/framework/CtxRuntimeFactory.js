sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxAdapterFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/usecases/CacheValidationUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/usecases/CacheReadUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/usecases/CacheWriteUseCase"
], function (
    CtxAdapterFactory,
    CacheValidationUseCase,
    CacheReadUseCase,
    CacheWriteUseCase
) {
    "use strict";

    function resolveDirectModel(oHost, sName) {
        if (!oHost || typeof oHost.getModel !== "function") {
            return null;
        }
        return oHost.getModel(sName);
    }

    function collectModels(oController) {
        if (!oController) {
            return {};
        }

        return {
            default: resolveDirectModel(oController),
            view: resolveDirectModel(oController, "view"),
            state: resolveDirectModel(oController, "state"),
            detail: resolveDirectModel(oController, "detail"),
            shell: resolveDirectModel(oController, "shell"),
            masterData: resolveDirectModel(oController, "masterData")
        };
    }

    function buildCacheRuntime() {
        return {
            cacheValidation: CacheValidationUseCase(),
            cacheRead: CacheReadUseCase(),
            cacheWrite: CacheWriteUseCase()
        };
    }

    function build(oController, mViewRefs) {
        var mModels = collectModels(oController);
        var mStateAdapters = CtxAdapterFactory.buildStateAdapters(mModels, mViewRefs);
        var oMasterDataModel = mModels.masterData;
        var oRuntimeSettings = oMasterDataModel && typeof oMasterDataModel.getProperty === "function"
            ? (
                oMasterDataModel.getProperty("/runtime/timers")
                || oMasterDataModel.getProperty("/runtime")
                || {}
            )
            : {};

        return Object.assign({},
            CtxAdapterFactory.buildInfraAdapters(mModels, mStateAdapters),
            mStateAdapters,
            {
                runtimeSettings: oRuntimeSettings
            },
            buildCacheRuntime()
        );
    }

    return {
        build: build
    };
});
