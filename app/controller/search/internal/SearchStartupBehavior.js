sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchStartupRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime"
], function (SearchStartupRuntime, SearchViewportRuntime, ControllerTextRuntime, ControllerViewStateRuntime, SearchCommandPolicy, SearchSelectionRuntime) {
    "use strict";

    function syncSmartControlAvailability(oController) {
        SearchSelectionRuntime.syncSearchTableRuntimeState(oController, SearchSelectionRuntime.resolveSearchInnerTable(oController));
        ControllerViewStateRuntime.set(oController, "/tableBusy", false);
    }

    function onSearchMatched(oController) {
        return SearchStartupRuntime.onSearchMatched(oController, {
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
            rebind: function (mInput) {
                return SearchCommandPolicy.rebind(oController, mInput);
            },
            rebindTableDirect: function () {
                var oSmartTable = oController && oController.byId && oController.byId("searchSmartTable");
                if (!oSmartTable || typeof oSmartTable.rebindTable !== "function") {
                    return false;
                }
                oSmartTable.rebindTable();
                return true;
            },
            restoreSearchScrollPosition: function () {
                SearchViewportRuntime.restoreSearchScrollPosition(oController);
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
