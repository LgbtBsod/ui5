sap.ui.define([], function () {
    "use strict";

    function initializeComponentRuntime(oComponent, mDeps, mModels, mOptions) {
        var CtxFactory = mDeps.CtxFactory;
        var DetailFacade = mDeps.DetailFacade;
        var ActionDispatcher = mDeps.ActionDispatcher;
        var ComponentRuntimeSupport = mDeps.ComponentRuntimeSupport;
        var EffectApplier = mDeps.EffectApplier;

        oComponent._ctx = CtxFactory.buildCtx(oComponent, {});
        oComponent._detailFacade = new DetailFacade();
        oComponent._actionDispatcher = new ActionDispatcher();

        return {
            resolveDetailCurrent: function () {
                return ComponentRuntimeSupport.resolveDetailCurrent(mModels.selectedModel);
            },
            buildLatestCtx: function () {
                oComponent._ctx = CtxFactory.buildCtx(oComponent, {});
                return oComponent._ctx;
            },
            applyFacadeResult: mOptions.createApplyFacadeResult({
                component: oComponent,
                effectApplier: EffectApplier,
                actionDispatcher: oComponent._actionDispatcher,
                selectedModel: mModels.selectedModel,
                uiStateModel: mModels.uiStateModel,
                componentRuntimeSupport: ComponentRuntimeSupport
            })
        };
    }

    return {
        initializeComponentRuntime: initializeComponentRuntime
    };
});
