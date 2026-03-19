sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchActionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchAnalyticsRailRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchStartupBehavior"
], function (SearchCommandPolicy, NavigationIntentService, SearchActionRuntime, SearchAnalyticsRailRuntime, SearchStartupBehavior) {
    "use strict";

    function syncSmartControlAvailability(oController) {
        return SearchStartupBehavior.syncSmartControlAvailability(oController);
    }

    function onSearchMatched(oController) {
        SearchAnalyticsRailRuntime.bindAnalyticsRefreshTimer(oController);
        return SearchStartupBehavior.onSearchMatched(oController);
    }

    function syncSearchContextForDetailRoute(oController) {
        return SearchStartupBehavior.syncSearchContextForDetailRoute(oController);
    }

    function openWorkflowAnalytics(oController) {
        return SearchActionRuntime.openWorkflowAnalytics(oController, {
            navigateToAnalytics: function () {
                NavigationIntentService.navigateToAnalytics(oController);
            }
        });
    }

    function closeWorkflowAnalytics(oController) {
        return SearchActionRuntime.closeWorkflowAnalytics(oController, {
            navigateBackFromAnalytics: function () {
                NavigationIntentService.navigateBackFromAnalytics(oController);
            }
        });
    }

    function runExport(oController, sEntity) {
        return SearchActionRuntime.runExport(oController, sEntity, {
            exportFlow: function (mInput) {
                return SearchCommandPolicy.exportFlow(oController, mInput);
            }
        });
    }

    return {
        closeWorkflowAnalytics: closeWorkflowAnalytics,
        onSearchMatched: onSearchMatched,
        openWorkflowAnalytics: openWorkflowAnalytics,
        runExport: runExport,
        syncSearchContextForDetailRoute: syncSearchContextForDetailRoute,
        syncSmartControlAvailability: syncSmartControlAvailability
    };
});
