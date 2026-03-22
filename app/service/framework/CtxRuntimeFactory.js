sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxModelResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxAdapterFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxCacheRuntimeFactory"
], function (
    CtxModelResolver,
    CtxAdapterFactory,
    CtxCacheRuntimeFactory
) {
    "use strict";

    function build(oController, mViewRefs) {
        var mModels = CtxModelResolver.collectModels(oController);
        var mStateAdapters = CtxAdapterFactory.buildStateAdapters(mModels, mViewRefs);

        return Object.assign({},
            CtxAdapterFactory.buildInfraAdapters(mModels, mStateAdapters),
            mStateAdapters,
            CtxCacheRuntimeFactory.buildCacheRuntime()
        );
    }

    return {
        build: build
    };
});
