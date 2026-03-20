sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/navigation/WorkspaceRouteNavigation"
], function (WorkspaceRouteNavigation) {
    "use strict";

    return Object.freeze({
        buildCurrentIntent: WorkspaceRouteNavigation.buildCurrentIntent,
        setAnalyticsReturnIntent: WorkspaceRouteNavigation.setAnalyticsReturnIntent,
        navigateToSearch: WorkspaceRouteNavigation.navigateToSearch,
        navigateToDetail: WorkspaceRouteNavigation.navigateToDetail,
        buildDetailHash: WorkspaceRouteNavigation.buildDetailHash,
        navigateToAnalytics: WorkspaceRouteNavigation.navigateToAnalytics,
        navigateBackFromAnalytics: WorkspaceRouteNavigation.navigateBackFromAnalytics
    });
});
