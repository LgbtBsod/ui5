sap.ui.define([
    "checklist/app/infra/navigation/WorkspaceRouteNavigation"
], function (WorkspaceRouteNavigation) {
    "use strict";

    function queuePendingIntent(oStateModel, StatePaths, oRouteEvent) {
        oStateModel.setProperty(StatePaths.PENDING_NAVIGATION_INTENT, {
            routeName: oRouteEvent && oRouteEvent.getParameter && oRouteEvent.getParameter("name"),
            routeArgs: (oRouteEvent && oRouteEvent.getParameter && oRouteEvent.getParameter("arguments")) || {},
            queuedAt: new Date().toISOString()
        });
    }

    function clearPendingIntent(oStateModel, StatePaths) {
        oStateModel.setProperty(StatePaths.PENDING_NAVIGATION_INTENT, null);
    }

    function resumePendingIntent(oComponent, oStateModel, StatePaths) {
        var oIntent = oStateModel.getProperty(StatePaths.PENDING_NAVIGATION_INTENT);
        if (!oIntent || !oIntent.routeName) {
            return false;
        }
        clearPendingIntent(oStateModel, StatePaths);
        oStateModel.setProperty("/navGuardBypass", true);
        oComponent.getRouter().navTo(oIntent.routeName, oIntent.routeArgs || {}, false);
        return true;
    }

    return {
        buildCurrentIntent: WorkspaceRouteNavigation.buildCurrentIntent,
        setAnalyticsReturnIntent: WorkspaceRouteNavigation.setAnalyticsReturnIntent,
        navigateToAnalytics: WorkspaceRouteNavigation.navigateToAnalytics,
        navigateBackFromAnalytics: WorkspaceRouteNavigation.navigateBackFromAnalytics,
        queuePendingIntent: queuePendingIntent,
        clearPendingIntent: clearPendingIntent,
        resumePendingIntent: resumePendingIntent
    };
});
