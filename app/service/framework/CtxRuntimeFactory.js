sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxAdapterFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/usecases/CacheValidationUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/usecases/CacheReadUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/usecases/CacheWriteUseCase"
], function (
    ControllerModelRuntime,
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
        var oView;

        if (!oController) {
            return {};
        }

        oView = ControllerModelRuntime.view(oController);

        return {
            default: ControllerModelRuntime.defaultModel(oController) || resolveDirectModel(oController),
            view: ControllerModelRuntime.viewState(oController) || (oView && oView.getModel ? oView.getModel("view") : null),
            state: ControllerModelRuntime.state(oController) || resolveDirectModel(oController, "state"),
            detail: ControllerModelRuntime.detail(oController) || resolveDirectModel(oController, "detail"),
            shell: ControllerModelRuntime.shell(oController) || resolveDirectModel(oController, "shell"),
            masterData: ControllerModelRuntime.masterData(oController) || resolveDirectModel(oController, "masterData")
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

        return Object.assign({},
            CtxAdapterFactory.buildInfraAdapters(mModels, mStateAdapters),
            mStateAdapters,
            buildCacheRuntime()
        );
    }

    return {
        build: build
    };
});
