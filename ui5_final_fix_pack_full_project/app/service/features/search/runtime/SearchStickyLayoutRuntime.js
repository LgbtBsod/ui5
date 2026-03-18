sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/SearchUiContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchStickyDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchStickyOffsetRuntime"
], function (SearchSelectionRuntime, ControllerViewStateRuntime, ThemeDomRuntime, SearchUiContracts, SearchStickyDomRuntime, SearchStickyOffsetRuntime) {
    "use strict";

    var SEARCH_STICKY_STACK_GAP_PX = SearchUiContracts.VIEWPORT.STICKY_STACK_GAP_PX;
    var SEARCH_SUMMARY_RAIL_GAP_PX = SearchUiContracts.VIEWPORT.SUMMARY_RAIL_GAP_PX;
    var SEARCH_MIN_HEADER_OFFSET_PX = SearchUiContracts.VIEWPORT.MIN_HEADER_OFFSET_PX;
    var SEARCH_HEADER_OFFSET_PADDING_PX = SearchUiContracts.VIEWPORT.HEADER_OFFSET_PADDING_PX;
    var SEARCH_MOBILE_STICKY_BREAKPOINT_PX = SearchUiContracts.VIEWPORT.MOBILE_STICKY_BREAKPOINT_PX;

    function syncSearchStickyOffsets(oController, oScrollHost) {
        var oViewDom = SearchStickyDomRuntime.resolveViewDom(oController);
        var oWorkbenchDock = SearchStickyDomRuntime.resolveSearchWorkbenchDock(oController);
        var oWorkbenchDom = oWorkbenchDock && oWorkbenchDock.getDomRef && oWorkbenchDock.getDomRef();
        var oFilterCard = oController.byId && oController.byId("searchFilterCard");
        var oActionRail = oController.byId && oController.byId("searchResultsActionRail");
        var oToolbarRail = oController.byId && oController.byId("smartTableCustomToolbar");
        var oSummaryRailDom = SearchStickyDomRuntime.resolveSearchSummaryRailDom(oController);
        var oResultsToolbarDom = SearchStickyDomRuntime.resolveSearchTableToolbarDom(oController);
        var iResultsToolbarHeight = SearchStickyDomRuntime.resolveResultsTableToolbarHeight(oController);
        var iSummaryRailHeight = SearchStickyDomRuntime.resolveDomHeight(oSummaryRailDom, ".searchResultsSummaryRail", oViewDom);
        var iFilterHeight = SearchStickyDomRuntime.resolveDomHeight(oFilterCard, ".searchFilterCardDense", oViewDom);
        var iActionHeight = SearchStickyDomRuntime.resolveDomHeight(oActionRail, ".searchResultsActionRail", oViewDom);
        var iToolbarHeight = SearchStickyDomRuntime.resolveDomHeight(oToolbarRail, ".searchSmartToolbarRail", oViewDom);
        var iDockHeight = SearchStickyDomRuntime.resolveOuterHeight(oWorkbenchDock);
        var iTopBase = SearchStickyOffsetRuntime.resolveShellHeaderOffset(SEARCH_MIN_HEADER_OFFSET_PX, SEARCH_HEADER_OFFSET_PADDING_PX, oScrollHost);
        var iActionTop;
        var iToolbarTop;
        var bCompactSticky = SearchStickyOffsetRuntime.isCompactStickyViewport(SEARCH_MOBILE_STICKY_BREAKPOINT_PX);
        if (!iDockHeight) {
            iDockHeight = iFilterHeight + iActionHeight + iToolbarHeight;
            if (iFilterHeight && iActionHeight) {
                iDockHeight += SEARCH_STICKY_STACK_GAP_PX;
            }
            if ((iFilterHeight || iActionHeight) && iToolbarHeight) {
                iDockHeight += SEARCH_STICKY_STACK_GAP_PX;
            }
        }
        iActionTop = iTopBase + iFilterHeight + (iFilterHeight && iActionHeight ? SEARCH_STICKY_STACK_GAP_PX : 0);
        iToolbarTop = iActionTop + iActionHeight + (iActionHeight && iToolbarHeight ? SEARCH_STICKY_STACK_GAP_PX : 0);
        ThemeDomRuntime.toggleClass([oWorkbenchDom], "searchWorkbenchDockCompactSticky", bCompactSticky);
        ThemeDomRuntime.toggleClass([oWorkbenchDom], "searchWorkbenchDockDesktopSticky", !bCompactSticky);
        ThemeDomRuntime.setStyleProperties([oWorkbenchDom], {
            "overflow": "visible",
            "overflow-x": "visible",
            "overflow-y": "visible"
        }, "important");
        SearchStickyDomRuntime.setSearchViewportCssVar(oController, "--search-sticky-dock-top", iTopBase + "px");
        SearchStickyDomRuntime.setSearchViewportCssVar(oController, "--search-workbench-toolbar-stack-height", (iActionHeight + iToolbarHeight + ((iActionHeight && iToolbarHeight) ? SEARCH_STICKY_STACK_GAP_PX : 0)) + "px");
        SearchStickyDomRuntime.setSearchViewportCssVar(oController, "--search-sticky-filter-top", iTopBase + "px");
        SearchStickyDomRuntime.setSearchViewportCssVar(oController, "--search-sticky-action-top", iActionTop + "px");
        SearchStickyDomRuntime.setSearchViewportCssVar(oController, "--search-sticky-toolbar-top", iToolbarTop + "px");
        SearchStickyDomRuntime.setSearchViewportCssVar(oController, "--search-results-toolbar-sticky-top", iToolbarTop + "px");
        SearchStickyDomRuntime.setSearchViewportCssVar(oController, "--search-outer-toolbar-height", iToolbarHeight + "px");
        SearchStickyDomRuntime.setSearchViewportCssVar(oController, "--search-summary-rail-height", iSummaryRailHeight + "px");
        SearchStickyDomRuntime.setSearchViewportCssVar(oController, "--search-summary-rail-sticky-top", iTopBase + "px");
        SearchStickyDomRuntime.setSearchViewportCssVar(
            oController,
            "--search-results-toolbar-compact-top",
            (iTopBase + iSummaryRailHeight + (iSummaryRailHeight ? SEARCH_SUMMARY_RAIL_GAP_PX : 0)) + "px"
        );
        SearchStickyDomRuntime.setSearchViewportCssVar(oController, "--search-smarttable-toolbar-height", iResultsToolbarHeight + "px");
        if (bCompactSticky) {
            SearchStickyDomRuntime.setSearchStaticTop(oFilterCard);
            SearchStickyDomRuntime.setSearchStaticTop(oActionRail);
            SearchStickyDomRuntime.setSearchStaticTop(oToolbarRail);
            SearchStickyDomRuntime.setSearchStickyTop(oSummaryRailDom, iTopBase + "px");
            SearchStickyDomRuntime.setSearchStickyTop(oResultsToolbarDom, (iTopBase + iSummaryRailHeight + (iSummaryRailHeight ? SEARCH_SUMMARY_RAIL_GAP_PX : 0)) + "px");
        } else {
            SearchStickyDomRuntime.setSearchStickyTop(oFilterCard, iTopBase + "px");
            SearchStickyDomRuntime.setSearchStickyTop(oActionRail, iActionTop + "px");
            SearchStickyDomRuntime.setSearchStickyTop(oToolbarRail, iToolbarTop + "px");
            SearchStickyDomRuntime.setSearchStaticTop(oSummaryRailDom);
            SearchStickyDomRuntime.setSearchStaticTop(oResultsToolbarDom);
        }
        ThemeDomRuntime.setStyleProperties([oFilterCard, oActionRail], {
            "position": "sticky",
            "overflow": "visible",
            "overflow-x": "visible",
            "overflow-y": "visible"
        }, "important");
        ThemeDomRuntime.setStyleProperties([oToolbarRail, oSummaryRailDom, oResultsToolbarDom], {
            "overflow": "visible",
            "overflow-x": "visible",
            "overflow-y": "visible"
        }, "important");
        if (bCompactSticky) {
            ThemeDomRuntime.setStyleProperties([oFilterCard, oActionRail], {
                "position": "relative",
                "top": "auto"
            }, "important");
            ThemeDomRuntime.setStyleProperty([oSummaryRailDom], "position", "sticky", "important");
            ThemeDomRuntime.setStyleProperty([oResultsToolbarDom], "position", "sticky", "important");
        } else {
            ThemeDomRuntime.setStyleProperties([oSummaryRailDom], {
                "position": "relative",
                "top": "auto"
            }, "important");
            ThemeDomRuntime.setStyleProperties([oResultsToolbarDom], {
                "position": "relative",
                "top": "auto"
            }, "important");
        }
    }

    function syncSearchViewportLayout(oController, oScrollHost) {
        var oInnerTable = SearchSelectionRuntime.resolveSearchInnerTable(oController);
        SearchSelectionRuntime.configureSearchResultTable(oController, oInnerTable, false);
        syncSearchStickyOffsets(oController, oScrollHost);
    }

    return {
        resolveSearchTableToolbarDom: SearchStickyDomRuntime.resolveSearchTableToolbarDom,
        resolveSearchWorkbenchDock: SearchStickyDomRuntime.resolveSearchWorkbenchDock,
        resolveShellHeaderOffset: function (_oController, oScrollHost) {
            return SearchStickyOffsetRuntime.resolveShellHeaderOffset(SEARCH_MIN_HEADER_OFFSET_PX, SEARCH_HEADER_OFFSET_PADDING_PX, oScrollHost);
        },
        resolveSearchViewportObserverTargets: SearchStickyDomRuntime.resolveSearchViewportObserverTargets,
        syncSearchStickyOffsets: syncSearchStickyOffsets,
        syncSearchViewportLayout: syncSearchViewportLayout
    };
});
