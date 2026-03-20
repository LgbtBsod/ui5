sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentActionRuntime"
], function (ComponentActionRuntime) {
    "use strict";

    function createPendingNavigationRuntime(oComponent, oStateModel, StatePaths, resumePendingNavigationIntent) {
        return {
            queuePendingNavigationIntent: function (oRouteEvent) {
                return ComponentActionRuntime.queuePendingNavigationIntent(oStateModel, StatePaths, oRouteEvent);
            },
            clearPendingNavigationIntent: function () {
                return ComponentActionRuntime.clearPendingNavigationIntent(oStateModel, StatePaths);
            },
            revertPendingNavigationIntent: function () {
                return ComponentActionRuntime.revertPendingNavigationIntent(oComponent, oStateModel, StatePaths);
            },
            restorePendingNavigationIntent: function () {
                return ComponentActionRuntime.restorePendingNavigationIntent(oComponent, oStateModel, StatePaths);
            },
            resumePendingNavigationIntent: function () {
                return resumePendingNavigationIntent(oComponent, oStateModel, StatePaths);
            }
        };
    }

    return {
        createPendingNavigationRuntime: createPendingNavigationRuntime
    };
});
