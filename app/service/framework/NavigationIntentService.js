sap.ui.define([
    "checklist/app/service/framework/behavior/BehaviorResolver",
    "checklist/app/service/framework/behavior/NavigationDefaultHandlers",
    "checklist/app/service/framework/behavior/NavigationOverrideHandlers"
], function (BehaviorResolver, NavigationDefaultHandlers, NavigationOverrideHandlers) {
    "use strict";

    function runOperation(sOperation, mContext) {
        NavigationDefaultHandlers.ensureRegistered();
        NavigationOverrideHandlers.ensureRegistered();
        return BehaviorResolver.executeSync("navigation", sOperation, mContext || {}, NavigationDefaultHandlers.handlers);
    }

    function buildCurrentIntent(oStateModel) {
        return runOperation("buildCurrentIntent", {
            stateModel: oStateModel
        });
    }

    function setAnalyticsReturnIntent(oController) {
        return runOperation("setAnalyticsReturnIntent", {
            controller: oController
        });
    }

    function navigateToAnalytics(oController) {
        return runOperation("navigateToAnalytics", {
            controller: oController
        });
    }

    function navigateBackFromAnalytics(oController) {
        return runOperation("navigateBackFromAnalytics", {
            controller: oController
        });
    }

    function queuePendingIntent(oStateModel, StatePaths, oRouteEvent) {
        return runOperation("queuePendingIntent", {
            stateModel: oStateModel,
            statePaths: StatePaths,
            routeEvent: oRouteEvent
        });
    }

    function clearPendingIntent(oStateModel, StatePaths) {
        return runOperation("clearPendingIntent", {
            stateModel: oStateModel,
            statePaths: StatePaths
        });
    }

    function resumePendingIntent(oComponent, oStateModel, StatePaths) {
        return runOperation("resumePendingIntent", {
            component: oComponent,
            stateModel: oStateModel,
            statePaths: StatePaths
        });
    }

    return {
        buildCurrentIntent: buildCurrentIntent,
        setAnalyticsReturnIntent: setAnalyticsReturnIntent,
        navigateToAnalytics: navigateToAnalytics,
        navigateBackFromAnalytics: navigateBackFromAnalytics,
        queuePendingIntent: queuePendingIntent,
        clearPendingIntent: clearPendingIntent,
        resumePendingIntent: resumePendingIntent,
        registerBehaviorOverride: NavigationOverrideHandlers.register,
        unregisterBehaviorOverride: NavigationOverrideHandlers.unregister,
        clearBehaviorOverrides: NavigationOverrideHandlers.clear
    };
});
