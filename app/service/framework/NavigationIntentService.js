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

    function navigateToSearch(oController) {
        return runOperation("navigateToSearch", {
            controller: oController
        });
    }

    function navigateToDetail(oController, sRootId, sLayout) {
        return runOperation("navigateToDetail", {
            controller: oController,
            rootId: sRootId,
            layout: sLayout
        });
    }

    function navigateToAccessDenied(oController, sRootId) {
        return runOperation("navigateToAccessDenied", {
            controller: oController,
            rootId: sRootId
        });
    }

    function buildDetailHash(oController, sRootId) {
        return runOperation("buildDetailHash", {
            controller: oController,
            rootId: sRootId
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
        buildDetailHash: buildDetailHash,
        setAnalyticsReturnIntent: setAnalyticsReturnIntent,
        navigateToAccessDenied: navigateToAccessDenied,
        navigateToAnalytics: navigateToAnalytics,
        navigateBackFromAnalytics: navigateBackFromAnalytics,
        navigateToDetail: navigateToDetail,
        navigateToSearch: navigateToSearch,
        queuePendingIntent: queuePendingIntent,
        clearPendingIntent: clearPendingIntent,
        resumePendingIntent: resumePendingIntent,
        registerBehaviorOverride: NavigationOverrideHandlers.register,
        unregisterBehaviorOverride: NavigationOverrideHandlers.unregister,
        clearBehaviorOverrides: NavigationOverrideHandlers.clear
    };
});
