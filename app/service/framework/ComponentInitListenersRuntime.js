sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentListenerStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentNavigationGuardRuntime"
], function (ComponentListenerStateRuntime, ComponentNavigationGuardRuntime) {
    "use strict";

    function syncDetailMeta(oStateModel, StatePaths) {
        return ComponentListenerStateRuntime.syncDetailMeta(oStateModel, StatePaths);
    }

    function attachInitListeners(mOptions) {
        var oStateModel = mOptions.stateModel;
        var StatePaths = mOptions.statePaths || {};

        ComponentListenerStateRuntime.attachLifecycleBindings(mOptions);
        ComponentListenerStateRuntime.initializeListenerState(mOptions);
        syncDetailMeta(oStateModel, StatePaths);
        ComponentNavigationGuardRuntime.attachBeforeRouteMatched({
            component: mOptions.component,
            stateModel: mOptions.stateModel,
            statePaths: mOptions.statePaths || {},
            flowCoordinator: mOptions.flowCoordinator,
            runGuardedSave: mOptions.runGuardedSave,
            queuePendingNavigationIntent: mOptions.queuePendingNavigationIntent,
            clearPendingNavigationIntent: mOptions.clearPendingNavigationIntent,
            revertPendingNavigationIntent: mOptions.revertPendingNavigationIntent,
            resumePendingNavigationIntent: mOptions.resumePendingNavigationIntent,
            restorePendingNavigationIntent: mOptions.restorePendingNavigationIntent,
            resetDetailAccessGuard: ComponentListenerStateRuntime.resetDetailAccessGuard,
            resetDetailNavigationState: ComponentListenerStateRuntime.resetDetailNavigationState
        });
    }

    return {
        attachInitListeners: attachInitListeners
    };
});
