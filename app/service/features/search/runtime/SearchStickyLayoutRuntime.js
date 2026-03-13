sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/SearchUiContracts"
], function (SearchSelectionRuntime, ControllerViewStateRuntime, ThemeDomRuntime, SearchUiContracts) {
    "use strict";

    var SEARCH_STICKY_STACK_GAP_PX = SearchUiContracts.VIEWPORT.STICKY_STACK_GAP_PX;
    var SEARCH_SUMMARY_RAIL_GAP_PX = SearchUiContracts.VIEWPORT.SUMMARY_RAIL_GAP_PX;
    var SEARCH_MIN_HEADER_OFFSET_PX = SearchUiContracts.VIEWPORT.MIN_HEADER_OFFSET_PX;
    var SEARCH_HEADER_OFFSET_PADDING_PX = SearchUiContracts.VIEWPORT.HEADER_OFFSET_PADDING_PX;
    var SEARCH_MOBILE_STICKY_BREAKPOINT_PX = SearchUiContracts.VIEWPORT.MOBILE_STICKY_BREAKPOINT_PX;

    function setSearchViewportCssVar(oController, sName, sValue) {
        var oViewDom = oController && oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
        ThemeDomRuntime.setStyleProperty([oViewDom], sName, sValue);
    }

    function resolveOuterHeight(oControl) {
        var oDomRef = oControl && oControl.getDomRef && oControl.getDomRef();
        if (!oDomRef || !oDomRef.getBoundingClientRect) {
            return 0;
        }
        return Math.max(0, Math.ceil(oDomRef.getBoundingClientRect().height || 0));
    }

    function resolveDomHeight(vControlOrDom, sSelectorFallback) {
        var oDomRef = vControlOrDom && vControlOrDom.nodeType ? vControlOrDom : (vControlOrDom && vControlOrDom.getDomRef && vControlOrDom.getDomRef());
        if ((!oDomRef || !oDomRef.getBoundingClientRect) && typeof document !== "undefined" && sSelectorFallback) {
            oDomRef = document.querySelector(sSelectorFallback);
        }
        if (!oDomRef || !oDomRef.getBoundingClientRect) {
            return 0;
        }
        return Math.max(0, Math.ceil(oDomRef.getBoundingClientRect().height || 0));
    }

    function resolveSearchTableToolbarDom(oController) {
        var oResultsShell = oController.byId && oController.byId("searchResultsShell");
        var oResultsShellDom = oResultsShell && oResultsShell.getDomRef && oResultsShell.getDomRef();
        return oResultsShellDom && oResultsShellDom.querySelector ? oResultsShellDom.querySelector(".sapUiCompSmartTableToolbar") : null;
    }

    function resolveSearchSummaryRailDom(oController) {
        var oSummaryRail = oController.byId && oController.byId("searchResultsSummaryRail");
        return oSummaryRail && oSummaryRail.getDomRef && oSummaryRail.getDomRef();
    }

    function setSearchStickyTop(vControlOrDom, sTop) {
        var oDomRef = vControlOrDom && vControlOrDom.nodeType ? vControlOrDom : (vControlOrDom && vControlOrDom.getDomRef && vControlOrDom.getDomRef());
        ThemeDomRuntime.setStyleProperty([oDomRef], "top", sTop);
        ThemeDomRuntime.setStyleProperty([oDomRef], "position", "sticky");
    }

    function setSearchStaticTop(vControlOrDom) {
        var oDomRef = vControlOrDom && vControlOrDom.nodeType ? vControlOrDom : (vControlOrDom && vControlOrDom.getDomRef && vControlOrDom.getDomRef());
        ThemeDomRuntime.setStyleProperties([oDomRef], {
            "position": "relative",
            "top": "auto"
        });
    }

    function isCompactStickyViewport() {
        return typeof window !== "undefined"
            && Number(window.innerWidth || 0) > 0
            && Number(window.innerWidth || 0) <= SEARCH_MOBILE_STICKY_BREAKPOINT_PX;
    }

    function resolveShellHeaderOffset(_oController, oScrollHost) {
        var oShellHeader = typeof document !== "undefined" ? document.querySelector(".appShellHeader") : null;
        var iShellBottom = 0;
        var iHostTop = 0;
        if (oShellHeader && oShellHeader.getBoundingClientRect) {
            iShellBottom = Math.ceil(oShellHeader.getBoundingClientRect().bottom || 0);
        }
        if (oScrollHost && oScrollHost.getBoundingClientRect) {
            iHostTop = Math.ceil(oScrollHost.getBoundingClientRect().top || 0);
        }
        return Math.max(SEARCH_MIN_HEADER_OFFSET_PX, iShellBottom - iHostTop + SEARCH_HEADER_OFFSET_PADDING_PX);
    }

    function resolveSearchWorkbenchDock(oController) {
        return oController.byId && oController.byId("searchWorkbenchDock");
    }

    function resolveResultsTableToolbarHeight(oController) {
        return resolveDomHeight(resolveSearchTableToolbarDom(oController));
    }

    function syncSearchStickyOffsets(oController, oScrollHost) {
        var oWorkbenchDock = resolveSearchWorkbenchDock(oController);
        var oWorkbenchDom = oWorkbenchDock && oWorkbenchDock.getDomRef && oWorkbenchDock.getDomRef();
        var oFilterCard = oController.byId && oController.byId("searchFilterCard");
        var oActionRail = oController.byId && oController.byId("searchResultsActionRail");
        var oToolbarRail = oController.byId && oController.byId("smartTableCustomToolbar");
        var oSummaryRailDom = resolveSearchSummaryRailDom(oController);
        var oResultsToolbarDom = resolveSearchTableToolbarDom(oController);
        var iResultsToolbarHeight = resolveResultsTableToolbarHeight(oController);
        var iSummaryRailHeight = resolveDomHeight(oSummaryRailDom, ".searchResultsSummaryRail");
        var iFilterHeight = resolveDomHeight(oFilterCard, ".searchFilterCardDense");
        var iActionHeight = resolveDomHeight(oActionRail, ".searchResultsActionRail");
        var iToolbarHeight = resolveDomHeight(oToolbarRail, ".searchSmartToolbarRail");
        var iDockHeight = resolveOuterHeight(oWorkbenchDock);
        var iTopBase = resolveShellHeaderOffset(oController, oScrollHost);
        var iActionTop;
        var iToolbarTop;
        var bCompactSticky = isCompactStickyViewport();
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
        setSearchViewportCssVar(oController, "--search-sticky-dock-top", iTopBase + "px");
        setSearchViewportCssVar(oController, "--search-workbench-toolbar-stack-height", (iActionHeight + iToolbarHeight + ((iActionHeight && iToolbarHeight) ? SEARCH_STICKY_STACK_GAP_PX : 0)) + "px");
        setSearchViewportCssVar(oController, "--search-sticky-filter-top", iTopBase + "px");
        setSearchViewportCssVar(oController, "--search-sticky-action-top", iActionTop + "px");
        setSearchViewportCssVar(oController, "--search-sticky-toolbar-top", iToolbarTop + "px");
        setSearchViewportCssVar(oController, "--search-results-toolbar-sticky-top", iToolbarTop + "px");
        setSearchViewportCssVar(oController, "--search-outer-toolbar-height", iToolbarHeight + "px");
        setSearchViewportCssVar(oController, "--search-summary-rail-height", iSummaryRailHeight + "px");
        setSearchViewportCssVar(oController, "--search-summary-rail-sticky-top", iTopBase + "px");
        setSearchViewportCssVar(
            oController,
            "--search-results-toolbar-compact-top",
            (iTopBase + iSummaryRailHeight + (iSummaryRailHeight ? SEARCH_SUMMARY_RAIL_GAP_PX : 0)) + "px"
        );
        setSearchViewportCssVar(oController, "--search-smarttable-toolbar-height", iResultsToolbarHeight + "px");
        if (bCompactSticky) {
            setSearchStaticTop(oFilterCard);
            setSearchStaticTop(oActionRail);
            setSearchStaticTop(oToolbarRail);
            setSearchStickyTop(oSummaryRailDom, iTopBase + "px");
            setSearchStickyTop(oResultsToolbarDom, (iTopBase + iSummaryRailHeight + (iSummaryRailHeight ? SEARCH_SUMMARY_RAIL_GAP_PX : 0)) + "px");
        } else {
            setSearchStickyTop(oFilterCard, iTopBase + "px");
            setSearchStickyTop(oActionRail, iActionTop + "px");
            setSearchStickyTop(oToolbarRail, iToolbarTop + "px");
            setSearchStaticTop(oSummaryRailDom);
            setSearchStickyTop(
                oResultsToolbarDom,
                (iToolbarTop + iToolbarHeight + (iToolbarHeight ? SEARCH_STICKY_STACK_GAP_PX : 0)) + "px"
            );
        }
        ThemeDomRuntime.setStyleProperties([oFilterCard, oActionRail, oResultsToolbarDom], {
            "position": "sticky",
            "overflow": "visible",
            "overflow-x": "visible",
            "overflow-y": "visible"
        }, "important");
        ThemeDomRuntime.setStyleProperties([oToolbarRail, oSummaryRailDom], {
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
        } else {
            ThemeDomRuntime.setStyleProperties([oSummaryRailDom], {
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

    function resolveSearchViewportObserverTargets(oController, oScrollHost) {
        var aTargets = [];
        var oViewDom = oController && oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
        var oWorkbenchDock = resolveSearchWorkbenchDock(oController);
        var oWorkbenchDom = oWorkbenchDock && oWorkbenchDock.getDomRef && oWorkbenchDock.getDomRef();
        var oFilterCard = oController.byId && oController.byId("searchFilterCard");
        var oActionRail = oController.byId && oController.byId("searchResultsActionRail");
        var oToolbarRail = oController.byId && oController.byId("smartTableCustomToolbar");
        var oResultsShell = oController.byId && oController.byId("searchResultsShell");
        var oShellHeader = typeof document !== "undefined" ? document.querySelector(".appShellHeader") : null;
        [oViewDom, oScrollHost, oWorkbenchDom, oShellHeader,
            oFilterCard && oFilterCard.getDomRef && oFilterCard.getDomRef(),
            oActionRail && oActionRail.getDomRef && oActionRail.getDomRef(),
            oToolbarRail && oToolbarRail.getDomRef && oToolbarRail.getDomRef(),
            resolveSearchSummaryRailDom(oController),
            oResultsShell && oResultsShell.getDomRef && oResultsShell.getDomRef(),
            resolveSearchTableToolbarDom(oController)
        ].forEach(function (oTarget) {
            if (oTarget && aTargets.indexOf(oTarget) < 0) {
                aTargets.push(oTarget);
            }
        });
        return aTargets;
    }

    return {
        resolveSearchTableToolbarDom: resolveSearchTableToolbarDom,
        resolveSearchWorkbenchDock: resolveSearchWorkbenchDock,
        resolveShellHeaderOffset: resolveShellHeaderOffset,
        resolveSearchViewportObserverTargets: resolveSearchViewportObserverTargets,
        syncSearchStickyOffsets: syncSearchStickyOffsets,
        syncSearchViewportLayout: syncSearchViewportLayout
    };
});
