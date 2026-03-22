sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentPendingNavigationRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentFacadeEffectRuntime"
], function (ComponentPendingNavigationRuntime, ComponentFacadeEffectRuntime) {
    "use strict";

    function queuePendingNavigationIntent(component, oStateModel, StatePaths, oRouteEvent, mIntentOptions) {
        ComponentPendingNavigationRuntime.createRuntime(component, oStateModel, StatePaths).queuePendingNavigationIntent(oRouteEvent, mIntentOptions);
    }

    function clearPendingNavigationIntent(component, oStateModel, StatePaths) {
        ComponentPendingNavigationRuntime.createRuntime(component, oStateModel, StatePaths).clearPendingNavigationIntent();
    }

    function revertPendingNavigationIntent(component, oStateModel, StatePaths) {
        return ComponentPendingNavigationRuntime.createRuntime(component, oStateModel, StatePaths).revertPendingNavigationIntent();
    }

    function resumePendingNavigationIntent(component, oStateModel, StatePaths) {
        return ComponentPendingNavigationRuntime.createRuntime(component, oStateModel, StatePaths).resumePendingNavigationIntent();
    }

    function restorePendingNavigationIntent(component, oStateModel, StatePaths) {
        return ComponentPendingNavigationRuntime.createRuntime(component, oStateModel, StatePaths).restorePendingNavigationIntent();
    }

    return {
        registerDefaultHandlers: ComponentFacadeEffectRuntime.registerDefaultHandlers,
        createBundleText: ComponentFacadeEffectRuntime.createBundleText,
        createApplyFacadeResult: ComponentFacadeEffectRuntime.createApplyFacadeResult,
        queuePendingNavigationIntent: queuePendingNavigationIntent,
        clearPendingNavigationIntent: clearPendingNavigationIntent,
        revertPendingNavigationIntent: revertPendingNavigationIntent,
        resumePendingNavigationIntent: resumePendingNavigationIntent,
        restorePendingNavigationIntent: restorePendingNavigationIntent
    };
});
