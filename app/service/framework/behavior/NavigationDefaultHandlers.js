sap.ui.define([
    "checklist/app/infra/navigation/WorkspaceRouteNavigation",
    "checklist/app/service/framework/behavior/BehaviorRegistry"
], function (WorkspaceRouteNavigation, BehaviorRegistry) {
    "use strict";

    var NAVIGATION_SCOPE = "navigation";
    var bDefaultsRegistered = false;

    function queuePendingIntent(mContext) {
        mContext.stateModel.setProperty(mContext.statePaths.PENDING_NAVIGATION_INTENT, {
            routeName: mContext.routeEvent && mContext.routeEvent.getParameter && mContext.routeEvent.getParameter("name"),
            routeArgs: (mContext.routeEvent && mContext.routeEvent.getParameter && mContext.routeEvent.getParameter("arguments")) || {},
            queuedAt: new Date().toISOString()
        });
    }

    function clearPendingIntent(mContext) {
        mContext.stateModel.setProperty(mContext.statePaths.PENDING_NAVIGATION_INTENT, null);
    }

    function resumePendingIntent(mContext) {
        var oIntent = mContext.stateModel.getProperty(mContext.statePaths.PENDING_NAVIGATION_INTENT);
        if (!oIntent || !oIntent.routeName) {
            return false;
        }
        clearPendingIntent(mContext);
        mContext.stateModel.setProperty("/navGuardBypass", true);
        mContext.component.getRouter().navTo(oIntent.routeName, oIntent.routeArgs || {}, false);
        return true;
    }

    var mHandlers = {
        buildCurrentIntent: function (mContext) {
            return WorkspaceRouteNavigation.buildCurrentIntent(mContext.stateModel);
        },
        setAnalyticsReturnIntent: function (mContext) {
            return WorkspaceRouteNavigation.setAnalyticsReturnIntent(mContext.controller);
        },
        navigateToAnalytics: function (mContext) {
            return WorkspaceRouteNavigation.navigateToAnalytics(mContext.controller);
        },
        navigateBackFromAnalytics: function (mContext) {
            return WorkspaceRouteNavigation.navigateBackFromAnalytics(mContext.controller);
        },
        queuePendingIntent: queuePendingIntent,
        clearPendingIntent: clearPendingIntent,
        resumePendingIntent: resumePendingIntent
    };

    function ensureRegistered() {
        if (bDefaultsRegistered) {
            return;
        }
        Object.keys(mHandlers).forEach(function (sOperation) {
            BehaviorRegistry.registerDefault(NAVIGATION_SCOPE, sOperation, mHandlers[sOperation]);
        });
        bDefaultsRegistered = true;
    }

    return {
        handlers: mHandlers,
        ensureRegistered: ensureRegistered
    };
});
