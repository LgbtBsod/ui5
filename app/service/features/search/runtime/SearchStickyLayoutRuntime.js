sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/AppShellDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchUiConstants"
], function (SearchSelectionRuntime, AppShellDomRuntime, ThemeDomRuntime, SearchUiContracts) {
    "use strict";

    var SEARCH_STICKY_STACK_GAP_PX = SearchUiContracts.VIEWPORT.STICKY_STACK_GAP_PX;
    var SEARCH_SUMMARY_RAIL_GAP_PX = SearchUiContracts.VIEWPORT.SUMMARY_RAIL_GAP_PX;
    var SEARCH_MIN_HEADER_OFFSET_PX = SearchUiContracts.VIEWPORT.MIN_HEADER_OFFSET_PX;
    var SEARCH_HEADER_OFFSET_PADDING_PX = SearchUiContracts.VIEWPORT.HEADER_OFFSET_PADDING_PX;
    var SEARCH_MOBILE_STICKY_BREAKPOINT_PX = SearchUiContracts.VIEWPORT.MOBILE_STICKY_BREAKPOINT_PX;

    function resolveViewDom(oController) {
        return oController && oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
    }

    function setSearchViewportCssVar(oController, sName, sValue) {
        ThemeDomRuntime.setStyleProperty([resolveViewDom(oController)], sName, sValue);
    }

    function resolveDomHeight(vControlOrDom) {
        var oDomRef = vControlOrDom && vControlOrDom.nodeType ? vControlOrDom : (vControlOrDom && vControlOrDom.getDomRef && vControlOrDom.getDomRef());
        if (!oDomRef || !oDomRef.getBoundingClientRect) {
            return 0;
        }
        return Math.max(0, Math.ceil(oDomRef.getBoundingClientRect().height || 0));
    }

    function resolveOuterHeight(oControl) {
        return resolveDomHeight(oControl);
    }

    function resolveSearchTableToolbarDom(oController) {
        var oToolbarHost = oController.byId && oController.byId("searchResultsToolbarHost");
        return oToolbarHost && oToolbarHost.getDomRef && oToolbarHost.getDomRef();
    }

    function resolveSearchSummaryRailDom(oController) {
        var oSummaryRail = oController.byId && oController.byId("searchResultsSummaryRail");
        return oSummaryRail && oSummaryRail.getDomRef && oSummaryRail.getDomRef();
    }

    function resolveSearchWorkbenchDock(oController) {
        return oController.byId && oController.byId("searchWorkbenchDock");
    }

    function resolveSearchActionRail(oController) {
        return (oController.byId && oController.byId("searchActionRailStack"))
            || (oController.byId && oController.byId("searchResultsActionRail"));
    }

    function resolveResultsTableToolbarHeight(oController) {
        return resolveDomHeight(resolveSearchTableToolbarDom(oController));
    }

    function resolveSearchViewportObserverTargets(oController, oScrollHost) {
        var aTargets = [];
        var oFilterCard = oController.byId && oController.byId("searchFilterCard");
        var oResultsShell = oController.byId && oController.byId("searchResultsShell");

        [
            resolveViewDom(oController),
            oScrollHost,
            AppShellDomRuntime.resolveShellHeaderHostDom(oController),
            oFilterCard && oFilterCard.getDomRef && oFilterCard.getDomRef(),
            resolveSearchSummaryRailDom(oController),
            oResultsShell && oResultsShell.getDomRef && oResultsShell.getDomRef()
        ].forEach(function (oTarget) {
            if (oTarget && aTargets.indexOf(oTarget) < 0) {
                aTargets.push(oTarget);
            }
        });

        return aTargets;
    }

    function readCssSizePx(sName) {
        var oRoot;
        var sValue;
        var iValue;
        if (typeof document === "undefined" || !document.documentElement || typeof window === "undefined" || typeof window.getComputedStyle !== "function") {
            return 0;
        }
        oRoot = document.documentElement;
        sValue = window.getComputedStyle(oRoot).getPropertyValue(sName) || "";
        iValue = parseFloat(String(sValue).trim());
        return isFinite(iValue) ? iValue : 0;
    }

    function isCompactStickyViewport(iBreakpointPx) {
        return typeof window !== "undefined"
            && Number(window.innerWidth || 0) > 0
            && Number(window.innerWidth || 0) <= iBreakpointPx;
    }

    function resolveShellHeaderOffset(iMinOffsetPx, iPaddingPx, oScrollHost) {
        var iShellOffset = Math.ceil(readCssSizePx("--app-shell-offset"));
        var iHostTop = 0;
        if (oScrollHost && oScrollHost.getBoundingClientRect) {
            iHostTop = Math.ceil(oScrollHost.getBoundingClientRect().top || 0);
        }
        return Math.max(iMinOffsetPx, Math.max(0, iShellOffset - iHostTop) + iPaddingPx);
    }

    function syncSearchStickyOffsets(oController, oScrollHost) {
        var oWorkbenchDock = resolveSearchWorkbenchDock(oController);
        var oWorkbenchDom = oWorkbenchDock && oWorkbenchDock.getDomRef && oWorkbenchDock.getDomRef();
        var oFilterCard = oController.byId && oController.byId("searchFilterCard");
        var oActionRail = resolveSearchActionRail(oController);
        var oSummaryRailDom = resolveSearchSummaryRailDom(oController);
        var oResultsToolbarDom = resolveSearchTableToolbarDom(oController);
        var iResultsToolbarHeight = resolveResultsTableToolbarHeight(oController);
        var iSummaryRailHeight = resolveDomHeight(oSummaryRailDom);
        var iFilterHeight = resolveDomHeight(oFilterCard);
        var iActionHeight = resolveDomHeight(oActionRail);
        var iToolbarHeight = resolveDomHeight(oResultsToolbarDom);
        var iDockHeight = resolveOuterHeight(oWorkbenchDock);
        var iTopBase = resolveShellHeaderOffset(SEARCH_MIN_HEADER_OFFSET_PX, SEARCH_HEADER_OFFSET_PADDING_PX, oScrollHost);
        var iActionTop;
        var iToolbarTop;
        var bCompactSticky = isCompactStickyViewport(SEARCH_MOBILE_STICKY_BREAKPOINT_PX);

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
        iToolbarTop = iTopBase + iSummaryRailHeight + (iSummaryRailHeight ? SEARCH_SUMMARY_RAIL_GAP_PX : 0);

        ThemeDomRuntime.toggleClass([oWorkbenchDom], "searchWorkbenchDockCompactSticky", bCompactSticky);
        ThemeDomRuntime.toggleClass([oWorkbenchDom], "searchWorkbenchDockDesktopSticky", !bCompactSticky);
        setSearchViewportCssVar(oController, "--search-sticky-dock-top", iTopBase + "px");
        setSearchViewportCssVar(oController, "--search-workbench-toolbar-stack-height", iActionHeight + "px");
        setSearchViewportCssVar(oController, "--search-sticky-filter-top", iTopBase + "px");
        setSearchViewportCssVar(oController, "--search-sticky-action-top", iActionTop + "px");
        setSearchViewportCssVar(oController, "--search-sticky-toolbar-top", iToolbarTop + "px");
        setSearchViewportCssVar(oController, "--search-results-toolbar-sticky-top", iToolbarTop + "px");
        setSearchViewportCssVar(oController, "--search-outer-toolbar-height", iResultsToolbarHeight + "px");
        setSearchViewportCssVar(oController, "--search-summary-rail-height", iSummaryRailHeight + "px");
        setSearchViewportCssVar(oController, "--search-summary-rail-sticky-top", iTopBase + "px");
        setSearchViewportCssVar(
            oController,
            "--search-results-toolbar-compact-top",
            (iTopBase + iSummaryRailHeight + (iSummaryRailHeight ? SEARCH_SUMMARY_RAIL_GAP_PX : 0)) + "px"
        );
        setSearchViewportCssVar(oController, "--search-smarttable-toolbar-height", iResultsToolbarHeight + "px");
    }

    function buildSearchViewportLayoutKey(oController, oScrollHost) {
        var oSummaryRailDom = resolveSearchSummaryRailDom(oController);
        var oResultsToolbarDom = resolveSearchTableToolbarDom(oController);
        var oFilterCard = oController.byId && oController.byId("searchFilterCard");
        var oActionRail = resolveSearchActionRail(oController);

        return [
            resolveShellHeaderOffset(SEARCH_MIN_HEADER_OFFSET_PX, SEARCH_HEADER_OFFSET_PADDING_PX, oScrollHost),
            resolveDomHeight(oSummaryRailDom),
            resolveDomHeight(oResultsToolbarDom),
            resolveDomHeight(oFilterCard),
            resolveDomHeight(oActionRail),
            isCompactStickyViewport(SEARCH_MOBILE_STICKY_BREAKPOINT_PX) ? "compact" : "desktop"
        ].join("|");
    }

    function syncSearchViewportLayout(oController, oScrollHost) {
        var oInnerTable = SearchSelectionRuntime.resolveSearchInnerTable(oController);
        SearchSelectionRuntime.configureSearchResultTable(oController, oInnerTable, false);
        syncSearchStickyOffsets(oController, oScrollHost);
    }

    return {
        resolveSearchTableToolbarDom: resolveSearchTableToolbarDom,
        resolveSearchWorkbenchDock: resolveSearchWorkbenchDock,
        resolveShellHeaderOffsetPx: resolveShellHeaderOffset,
        resolveShellHeaderOffset: function (_oController, oScrollHost) {
            return resolveShellHeaderOffset(SEARCH_MIN_HEADER_OFFSET_PX, SEARCH_HEADER_OFFSET_PADDING_PX, oScrollHost);
        },
        resolveSearchViewportObserverTargets: resolveSearchViewportObserverTargets,
        buildSearchViewportLayoutKey: buildSearchViewportLayoutKey,
        syncSearchStickyOffsets: syncSearchStickyOffsets,
        syncSearchViewportLayout: syncSearchViewportLayout
    };
});
