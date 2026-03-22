sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService"
], function (
    NavigationIntentService
) {
    "use strict";

    function createRuntime(oComponent, oStateModel, StatePaths) {
        return {
            queuePendingNavigationIntent: function (oRouteEvent, mIntentOptions) {
                NavigationIntentService.queuePendingIntent(oComponent, oStateModel, StatePaths, oRouteEvent, mIntentOptions);
            },
            clearPendingNavigationIntent: function () {
                NavigationIntentService.clearPendingIntent(oStateModel, StatePaths);
            },
            revertPendingNavigationIntent: function () {
                return NavigationIntentService.revertPendingIntent(oComponent, oStateModel, StatePaths);
            },
            resumePendingNavigationIntent: function () {
                return NavigationIntentService.resumePendingIntent(oComponent, oStateModel, StatePaths);
            },
            restorePendingNavigationIntent: function () {
                return NavigationIntentService.restorePendingIntent(oComponent, oStateModel, StatePaths);
            }
        };
    }

    return {
        createRuntime: createRuntime
    };
});
