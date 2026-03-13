sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchStartupRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchAnalyticsRailRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime"
], function (SearchStartupRuntime, SearchAnalyticsRailRuntime, SearchViewportRuntime, ControllerTextRuntime, ControllerViewStateRuntime, SearchCommandPolicy, SearchSelectionRuntime) {
    "use strict";

    function syncSmartControlAvailability(oController) {
        SearchSelectionRuntime.syncSearchTableRuntimeState(oController, SearchSelectionRuntime.resolveSearchInnerTable(oController));
        ControllerViewStateRuntime.set(oController, "/tableBusy", false);
    }

    function onSearchMatched(oController) {
        return SearchStartupRuntime.onSearchMatched(oController, {
            bindSearchAnalytics: function () {
                oController._runSearchAnalytics = function (mInput) {
                    return SearchCommandPolicy.analytics(oController, mInput);
                };
            },
            bindSearchViewportRuntime: function () {
                SearchViewportRuntime.bindSearchViewportRuntime(oController);
            },
            bindSearchWorkingText: function () {
                oController._resolveSearchWorkingText = function () {
                    return ControllerTextRuntime.getText(oController, "workingMessageLong", [], "Working...");
                };
            },
            bootstrap: function (mInput) {
                return SearchCommandPolicy.bootstrap(oController, mInput);
            },
            clearInitialAnalyticsSchedule: function () {
                SearchAnalyticsRailRuntime.clearInitialAnalyticsSchedule(oController);
            },
            rebind: function (mInput) {
                return SearchCommandPolicy.rebind(oController, mInput);
            },
            restoreSearchScrollPosition: function () {
                SearchViewportRuntime.restoreSearchScrollPosition(oController);
            },
            scheduleInitialAnalytics: function (fnAfterStart) {
                return SearchAnalyticsRailRuntime.scheduleInitialAnalytics(oController, fnAfterStart);
            },
            syncSmartControlAvailability: function () {
                syncSmartControlAvailability(oController);
            }
        });
    }

    function syncSearchContextForDetailRoute(oController) {
        return SearchStartupRuntime.syncSearchContextForDetailRoute(oController, {
            bindSearchViewportRuntime: function () {
                SearchViewportRuntime.bindSearchViewportRuntime(oController);
            },
            scheduleSearchViewportSync: function (bImmediate) {
                SearchViewportRuntime.scheduleSearchViewportSync(oController, bImmediate);
            },
            syncSmartControlAvailability: function () {
                syncSmartControlAvailability(oController);
            }
        });
    }

    return {
        onSearchMatched: onSearchMatched,
        syncSearchContextForDetailRoute: syncSearchContextForDetailRoute,
        syncSmartControlAvailability: syncSmartControlAvailability
    };
});
