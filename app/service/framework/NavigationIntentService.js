sap.ui.define([], function () {
    "use strict";

    function navigationScope() {
        return sap.ui.requireSync("PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorScopes").navigation;
    }

    function runOperation(sOperation, mContext) {
        return navigationScope().executeSync(sOperation, mContext || {});
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

    function queuePendingIntent(oComponent, oStateModel, StatePaths, oRouteEvent, mIntentOptions) {
        return runOperation("queuePendingIntent", {
            component: oComponent,
            stateModel: oStateModel,
            statePaths: StatePaths,
            routeEvent: oRouteEvent,
            owner: mIntentOptions && mIntentOptions.owner,
            resumeMode: mIntentOptions && mIntentOptions.resumeMode
        });
    }

    function clearPendingIntent(oStateModel, StatePaths) {
        return runOperation("clearPendingIntent", {
            stateModel: oStateModel,
            statePaths: StatePaths
        });
    }

    function revertPendingIntent(oComponent, oStateModel, StatePaths) {
        return runOperation("revertPendingIntent", {
            component: oComponent,
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

    function restorePendingIntent(oComponent, oStateModel, StatePaths) {
        return runOperation("restorePendingIntent", {
            component: oComponent,
            stateModel: oStateModel,
            statePaths: StatePaths
        });
    }

    return {
        buildCurrentIntent: buildCurrentIntent,
        buildDetailHash: buildDetailHash,
        setAnalyticsReturnIntent: setAnalyticsReturnIntent,
        navigateToAnalytics: navigateToAnalytics,
        navigateBackFromAnalytics: navigateBackFromAnalytics,
        navigateToDetail: navigateToDetail,
        navigateToSearch: navigateToSearch,
        queuePendingIntent: queuePendingIntent,
        clearPendingIntent: clearPendingIntent,
        revertPendingIntent: revertPendingIntent,
        resumePendingIntent: resumePendingIntent,
        restorePendingIntent: restorePendingIntent,
        registerBehaviorOverride: function () {
            return navigationScope().registerBehaviorOverride.apply(null, arguments);
        },
        unregisterBehaviorOverride: function () {
            return navigationScope().unregisterBehaviorOverride.apply(null, arguments);
        },
        clearBehaviorOverrides: function () {
            return navigationScope().clearBehaviorOverrides.apply(null, arguments);
        }
    };
});
