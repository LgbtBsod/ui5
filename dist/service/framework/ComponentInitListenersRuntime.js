sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentDetailMetaSyncRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentListenerBindingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentListenerInitRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentNavigationGuardRuntime"
], function (ComponentDetailMetaSyncRuntime, ComponentListenerBindingRuntime, ComponentListenerInitRuntime, ComponentNavigationGuardRuntime) {
    "use strict";

    function syncDetailMeta(oStateModel, StatePaths) {
        return ComponentDetailMetaSyncRuntime.syncDetailMeta(oStateModel, StatePaths);
    }

    function attachInitListeners(mOptions) {
        var oStateModel = mOptions.stateModel;
        var StatePaths = mOptions.statePaths || {};

        ComponentListenerBindingRuntime.attachLifecycleBindings(mOptions);
        ComponentListenerInitRuntime.initializeListeners(mOptions);
        syncDetailMeta(oStateModel, StatePaths);
        ComponentNavigationGuardRuntime.attachBeforeRouteMatched({
            component: mOptions.component,
            stateModel: mOptions.stateModel,
            statePaths: mOptions.statePaths || {},
            workflowCoordinator: mOptions.workflowCoordinator,
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
