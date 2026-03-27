sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentDetailMetaSyncRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentListenerInitRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentLockEventsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentNavigationGuardDecisionRuntime"
], function (ComponentDetailMetaSyncRuntime, ComponentListenerInitRuntime, ComponentLockEventsRuntime, ComponentNavigationGuardDecisionRuntime) {
    "use strict";

    function syncDetailMeta(oStateModel, StatePaths) {
        return ComponentDetailMetaSyncRuntime.syncDetailMeta(oStateModel, StatePaths);
    }

    function attachBeforeRouteMatched(mOptions) {
        var oComponent = mOptions.component;
        var oRouter = oComponent.getRouter();

        if (oComponent._oLifecycleRouter && oComponent._fnBeforeRouteMatched && oComponent._oLifecycleRouter.detachBeforeRouteMatched) {
            oComponent._oLifecycleRouter.detachBeforeRouteMatched(oComponent._fnBeforeRouteMatched, oComponent);
        }

        oComponent._oLifecycleRouter = oRouter;
        oComponent._fnBeforeRouteMatched = function (oEvent) {
            return ComponentNavigationGuardDecisionRuntime.handleBeforeRouteMatched(mOptions, oEvent);
        };

        oRouter.attachBeforeRouteMatched(oComponent._fnBeforeRouteMatched, oComponent);
    }

    function attachInitListeners(mOptions) {
        var oStateModel = mOptions.stateModel;
        var StatePaths = mOptions.statePaths || {};

        ComponentLockEventsRuntime.attachLifecycleBindings(mOptions);
        ComponentListenerInitRuntime.initializeListeners(mOptions);
        syncDetailMeta(oStateModel, StatePaths);
        attachBeforeRouteMatched({
            component: mOptions.component,
            stateModel: mOptions.stateModel,
            statePaths: mOptions.statePaths || {},
            runGuardedSave: mOptions.runGuardedSave,
            queuePendingNavigationIntent: mOptions.queuePendingNavigationIntent,
            clearPendingNavigationIntent: mOptions.clearPendingNavigationIntent,
            revertPendingNavigationIntent: mOptions.revertPendingNavigationIntent,
            resumePendingNavigationIntent: mOptions.resumePendingNavigationIntent,
            restorePendingNavigationIntent: mOptions.restorePendingNavigationIntent,
            resetDetailAccessGuard: ComponentDetailMetaSyncRuntime.resetDetailAccessGuard,
            resetDetailNavigationState: ComponentDetailMetaSyncRuntime.resetDetailNavigationState
        });
    }

    return {
        attachInitListeners: attachInitListeners
    };
});
