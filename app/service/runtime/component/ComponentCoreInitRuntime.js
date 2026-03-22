sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxRuntimeFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentFacadeEffectRuntime"
], function (CtxRuntimeFactory, ComponentFacadeEffectRuntime) {
    "use strict";

    function initializeComponentRuntime(oComponent, mDeps, mModels) {
        var DetailFacade = mDeps.DetailFacade;
        var ActionDispatcher = mDeps.ActionDispatcher;
        var ComponentRuntimeSupport = mDeps.ComponentRuntimeSupport;
        var EffectApplier = mDeps.EffectApplier;

        oComponent._ctx = CtxRuntimeFactory.build(oComponent, {});
        oComponent._detailFacade = new DetailFacade();
        oComponent._actionDispatcher = new ActionDispatcher();

        return {
            resolveDetailCurrent: function () {
                return ComponentRuntimeSupport.resolveDetailCurrent(mModels.detailModel);
            },
            buildLatestCtx: function () {
                oComponent._ctx = CtxRuntimeFactory.build(oComponent, {});
                return oComponent._ctx;
            },
            applyFacadeResult: ComponentFacadeEffectRuntime.createApplyFacadeResult({
                component: oComponent,
                effectApplier: EffectApplier,
                actionDispatcher: oComponent._actionDispatcher,
                shellModel: mModels.shellModel,
                componentRuntimeSupport: ComponentRuntimeSupport
            })
        };
    }

    return {
        initializeComponentRuntime: initializeComponentRuntime
    };
});
