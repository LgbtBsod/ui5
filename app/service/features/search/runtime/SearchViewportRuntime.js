sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchScrollRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchStickyLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewportBindingRuntime"
], function (SearchScrollRuntime, SearchStickyLayoutRuntime, SearchViewportBindingRuntime) {
    "use strict";

    function createViewportHooks(oController) {
        return {
            resolveToolbarDom: function () {
                return SearchStickyLayoutRuntime.resolveSearchTableToolbarDom(oController);
            },
            syncViewportLayout: function () {
                SearchViewportBindingRuntime.syncSearchViewportLayout(oController);
            }
        };
    }

    function captureSearchScrollPosition(oController) {
        SearchScrollRuntime.captureSearchScrollPosition(oController);
    }

    function restoreSearchScrollPosition(oController) {
        SearchScrollRuntime.restoreSearchScrollPosition(oController, createViewportHooks(oController));
    }

    function scrollToSearchFilters(oController) {
        var oFilterCard = oController.byId && oController.byId("searchFilterCard");
        var oFilterDom = oFilterCard && oFilterCard.getDomRef && oFilterCard.getDomRef();
        return SearchScrollRuntime.scrollToTarget(
            oController,
            oFilterDom,
            SearchStickyLayoutRuntime.resolveShellHeaderOffset(oController, SearchScrollRuntime.resolveSearchScrollHost(oController)),
            createViewportHooks(oController)
        );
    }

    function scrollToSearchResultsToolbar(oController) {
        var oResultsShell = oController.byId && oController.byId("searchResultsShell");
        var oResultsShellDom = oResultsShell && oResultsShell.getDomRef && oResultsShell.getDomRef();
        var oResultsToolbarDom = SearchStickyLayoutRuntime.resolveSearchTableToolbarDom(oController);
        var oToolbarDom = oResultsToolbarDom || oResultsShellDom;
        return SearchScrollRuntime.scrollToTarget(
            oController,
            oToolbarDom,
            SearchStickyLayoutRuntime.resolveShellHeaderOffset(oController, SearchScrollRuntime.resolveSearchScrollHost(oController)),
            createViewportHooks(oController)
        );
    }

    return {
        bindSearchViewportRuntime: SearchViewportBindingRuntime.bindSearchViewportRuntime,
        captureSearchScrollPosition: captureSearchScrollPosition,
        restoreSearchScrollPosition: restoreSearchScrollPosition,
        scheduleSearchViewportSync: SearchViewportBindingRuntime.scheduleSearchViewportSync,
        scrollToSearchFilters: scrollToSearchFilters,
        scrollToSearchResultsToolbar: scrollToSearchResultsToolbar,
        syncSearchScrollAffordances: SearchViewportBindingRuntime.syncSearchScrollAffordances,
        syncSearchViewportLayout: SearchViewportBindingRuntime.syncSearchViewportLayout,
        unbindSearchViewportRuntime: SearchViewportBindingRuntime.unbindSearchViewportRuntime
    };
});
