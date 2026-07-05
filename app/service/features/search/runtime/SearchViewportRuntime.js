sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EventDelegateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiControlIds",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchViewportContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/AppShellDomRuntime"
], function (
    ModelStateRuntime,
    SchedulingRuntime,
    EventDelegateRuntime,
    ThemeDomRuntime,
    SearchUiContracts,
    JsRuntime,
    UiControlIds,
    SearchViewportContracts,
    SearchSelectionRuntime,
    AppShellDomRuntime
) {
    "use strict";

    var VIEWPORT = SearchUiContracts.VIEWPORT;
    var SEARCH_ANCHOR_SCROLL_MARGIN_PX = VIEWPORT.ANCHOR_SCROLL_MARGIN_PX;
    var SEARCH_STICKY_STACK_GAP_PX = VIEWPORT.STICKY_STACK_GAP_PX;
    var SEARCH_SUMMARY_RAIL_GAP_PX = VIEWPORT.SUMMARY_RAIL_GAP_PX;
    var SEARCH_MIN_HEADER_OFFSET_PX = VIEWPORT.MIN_HEADER_OFFSET_PX;
    var SEARCH_HEADER_OFFSET_PADDING_PX = VIEWPORT.HEADER_OFFSET_PADDING_PX;
    var SEARCH_MOBILE_STICKY_BREAKPOINT_PX = VIEWPORT.MOBILE_STICKY_BREAKPOINT_PX;
    var SEARCH_VIEWPORT_LAYOUT_DEBOUNCE_MS = VIEWPORT.LAYOUT_DEBOUNCE_MS;
    var STATE_MODEL = SearchViewportContracts.MODELS.STATE;
    var STATE_PATHS = SearchViewportContracts.STATE_PATHS;
    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;

    function getStateModel(oController) {
        return oController && oController.getModel ? oController.getModel(STATE_MODEL) : null;
    }

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
        var oFilterCard = oController.byId && oController.byId(UiControlIds.SEARCH.FILTER_CARD);
        var oResultsShell = oController.byId && oController.byId(UiControlIds.SEARCH.RESULTS_SHELL);

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

    /* Использует ширину реального DOM-контейнера вью, а не window.innerWidth:
     * в двух-колоночном FlexibleColumnLayout (Search + Detail/Analytics) окно
     * остаётся широким, но колонка Search может сжаться до мобильной ширины —
     * ранее компакт-режим в этом случае не активировался вовсе. */
    function isCompactStickyViewport(oController) {
        var oViewDom = resolveViewDom(oController);
        var iWidth = oViewDom && oViewDom.getBoundingClientRect
            ? oViewDom.getBoundingClientRect().width
            : (typeof window !== "undefined" ? window.innerWidth : 0);
        return Number(iWidth || 0) > 0 && Number(iWidth || 0) <= SEARCH_MOBILE_STICKY_BREAKPOINT_PX;
    }

    function resolveShellHeaderOffset(oController, oScrollHost) {
        var iShellOffset = Math.ceil(readCssSizePx("--app-shell-offset"));
        var iHostTop = 0;
        if (oScrollHost && oScrollHost.getBoundingClientRect) {
            iHostTop = Math.ceil(oScrollHost.getBoundingClientRect().top || 0);
        }
        return Math.max(SEARCH_MIN_HEADER_OFFSET_PX, Math.max(0, iShellOffset - iHostTop) + SEARCH_HEADER_OFFSET_PADDING_PX);
    }

    function syncSearchStickyOffsets(oController, oScrollHost) {
        var oWorkbenchDock = resolveSearchWorkbenchDock(oController);
        var oWorkbenchDom = oWorkbenchDock && oWorkbenchDock.getDomRef && oWorkbenchDock.getDomRef();
        var oFilterCard = oController.byId && oController.byId(UiControlIds.SEARCH.FILTER_CARD);
        var oActionRail = resolveSearchActionRail(oController);
        var oSummaryRailDom = resolveSearchSummaryRailDom(oController);
        var oResultsToolbarDom = resolveSearchTableToolbarDom(oController);
        var iResultsToolbarHeight = resolveResultsTableToolbarHeight(oController);
        var iSummaryRailHeight = resolveDomHeight(oSummaryRailDom);
        var iFilterHeight = resolveDomHeight(oFilterCard);
        var iActionHeight = resolveDomHeight(oActionRail);
        var iToolbarHeight = resolveDomHeight(oResultsToolbarDom);
        var iDockHeight = resolveOuterHeight(oWorkbenchDock);
        var iTopBase = resolveShellHeaderOffset(oController, oScrollHost);
        var iActionTop;
        var iToolbarTop;
        var bCompactSticky = isCompactStickyViewport(oController);

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
        var oFilterCard = oController.byId && oController.byId(UiControlIds.SEARCH.FILTER_CARD);
        var oActionRail = resolveSearchActionRail(oController);

        return [
            resolveShellHeaderOffset(oController, oScrollHost),
            resolveDomHeight(oSummaryRailDom),
            resolveDomHeight(oResultsToolbarDom),
            resolveDomHeight(oFilterCard),
            resolveDomHeight(oActionRail),
            isCompactStickyViewport(oController) ? "compact" : "desktop"
        ].join("|");
    }

    function resolveSearchScrollHost(oController) {
        var oDomRef = oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
        var oNode = oDomRef && oDomRef.parentElement;
        var oDocumentScrollHost;
        while (oNode && oNode !== document.body) {
            if (oNode.scrollHeight > oNode.clientHeight + 4) {
                return oNode;
            }
            oNode = oNode.parentElement;
        }
        oDocumentScrollHost = document.scrollingElement || document.documentElement || document.body;
        if (oDocumentScrollHost && oDocumentScrollHost.scrollHeight > oDocumentScrollHost.clientHeight + 4) {
            return oDocumentScrollHost;
        }
        return oDocumentScrollHost || null;
    }

    function syncSearchViewportLayout(oController, bForce) {
        var oScrollHost = resolveSearchScrollHost(oController);
        var sLayoutKey = buildSearchViewportLayoutKey(oController, oScrollHost);
        if (!bForce && sLayoutKey && oController._sSearchTableLayoutKey === sLayoutKey) {
            return false;
        }
        syncSearchStickyOffsets(oController, oScrollHost);
        oController._sSearchTableLayoutKey = sLayoutKey;
        return true;
    }

    function clearSearchViewportSyncTimer(oController) {
        oController._iSearchViewportSyncTimer = SchedulingRuntime.clearTimer(oController._iSearchViewportSyncTimer);
    }

    function flushSearchViewportSync(oController) {
        oController._iSearchViewportSyncRaf = SchedulingRuntime.requestFrameOnce(oController._iSearchViewportSyncRaf, function () {
            oController._iSearchViewportSyncRaf = 0;
            syncSearchViewportLayout(oController, false);
        });
    }

    function scheduleSearchViewportSync(oController, bImmediate) {
        clearSearchViewportSyncTimer(oController);
        if (bImmediate) {
            flushSearchViewportSync(oController);
            return;
        }
        oController._iSearchViewportSyncTimer = SchedulingRuntime.restartTimer(0, function () {
            oController._iSearchViewportSyncTimer = 0;
            flushSearchViewportSync(oController);
        }, SEARCH_VIEWPORT_LAYOUT_DEBOUNCE_MS);
    }

    function bindSearchViewportObservers(oController, oScrollHost) {
        var aPrevTargets;
        var aNextTargets;
        if (typeof window === "undefined" || typeof window.ResizeObserver !== TYPE_FUNCTION) {
            return;
        }
        if (!oController._oSearchViewportResizeObserver) {
            oController._oSearchViewportResizeObserver = new window.ResizeObserver(function () {
                scheduleSearchViewportSync(oController, false);
            });
        }
        aPrevTargets = oController._aSearchViewportObserverTargets || [];
        aNextTargets = resolveSearchViewportObserverTargets(oController, oScrollHost);
        aPrevTargets.forEach(function (oTarget) {
            if (aNextTargets.indexOf(oTarget) < 0) {
                oController._oSearchViewportResizeObserver.unobserve(oTarget);
            }
        });
        aNextTargets.forEach(function (oTarget) {
            if (aPrevTargets.indexOf(oTarget) < 0) {
                oController._oSearchViewportResizeObserver.observe(oTarget);
            }
        });
        oController._aSearchViewportObserverTargets = aNextTargets;
    }

    function unbindSearchViewportObservers(oController) {
        var oObserver = oController._oSearchViewportResizeObserver;
        (oController._aSearchViewportObserverTargets || []).forEach(function (oTarget) {
            if (oObserver) {
                oObserver.unobserve(oTarget);
            }
        });
        if (oObserver && oObserver.disconnect) {
            oObserver.disconnect();
        }
        oController._oSearchViewportResizeObserver = null;
        oController._aSearchViewportObserverTargets = null;
    }

    function bindSearchViewportRuntime(oController) {
        var oView = oController && oController.getView && oController.getView();
        var oScrollHost = resolveSearchScrollHost(oController);
        if (!oView) {
            return;
        }
        if (!oController._oSearchViewportDelegate) {
            oController._oSearchViewportDelegate = {
                onAfterRendering: function () {
                    bindSearchViewportRuntime(oController);
                }
            };
            EventDelegateRuntime.ensure(oController, "_oSearchViewportDelegate", oView, oController._oSearchViewportDelegate, oController);
        }
        if (oController._oSearchScrollHost === oScrollHost) {
            bindSearchViewportObservers(oController, oScrollHost);
            syncSearchViewportLayout(oController, false);
            return;
        }
        oController._oSearchScrollHost = oScrollHost;
        if (oController._oSearchScrollSyncHost && oController._oSearchScrollSyncHost !== oScrollHost && oController._fnSearchScrollSync) {
            oController._oSearchScrollSyncHost.removeEventListener("scroll", oController._fnSearchScrollSync);
        }
        if (!oController._fnSearchViewportResize) {
            oController._fnSearchViewportResize = function () {
                scheduleSearchViewportSync(oController, false);
            };
            window.addEventListener("resize", oController._fnSearchViewportResize);
        }
        if (oScrollHost && !oController._fnSearchScrollSync) {
            oController._fnSearchScrollSync = function () {
                scheduleSearchViewportSync(oController, false);
            };
            oScrollHost.addEventListener("scroll", oController._fnSearchScrollSync, { passive: true });
            oController._oSearchScrollSyncHost = oScrollHost;
        } else if (oScrollHost && oController._oSearchScrollSyncHost !== oScrollHost && oController._fnSearchScrollSync) {
            oScrollHost.addEventListener("scroll", oController._fnSearchScrollSync, { passive: true });
            oController._oSearchScrollSyncHost = oScrollHost;
        }
        bindSearchViewportObservers(oController, oScrollHost);
        syncSearchViewportLayout(oController, true);
    }

    function unbindSearchViewportRuntime(oController) {
        var oView = oController && oController.getView && oController.getView();
        if (oView && oController._oSearchViewportDelegate && oView.removeEventDelegate) {
            EventDelegateRuntime.remove(oController, "_oSearchViewportDelegate", oView);
        }
        if (oController._fnSearchViewportResize) {
            window.removeEventListener("resize", oController._fnSearchViewportResize);
        }
        if (oController._oSearchScrollSyncHost && oController._fnSearchScrollSync) {
            oController._oSearchScrollSyncHost.removeEventListener("scroll", oController._fnSearchScrollSync);
        }
        unbindSearchViewportObservers(oController);
        clearSearchViewportSyncTimer(oController);
        oController._iSearchAnchorSyncTimer = SchedulingRuntime.clearTimer(oController._iSearchAnchorSyncTimer);
        oController._iSearchViewportSyncRaf = SchedulingRuntime.clearFrame(oController._iSearchViewportSyncRaf);
        oController._oSearchScrollHost = null;
        oController._oSearchScrollSyncHost = null;
        oController._fnSearchScrollSync = null;
        oController._fnSearchViewportResize = null;
        oController._sSearchTableLayoutKey = "";
        oController._iSearchViewportSyncTimer = 0;
        oController._iSearchAnchorSyncTimer = 0;
        oController._iSearchViewportSyncRaf = 0;
    }

    function captureSearchScrollPosition(oController) {
        var oScrollHost = resolveSearchScrollHost(oController);
        if (!getStateModel(oController)) {
            return;
        }
        ModelStateRuntime.write(oController, STATE_MODEL, STATE_PATHS.SEARCH_SCROLL_STATE, {
            hostTop: oScrollHost ? oScrollHost.scrollTop : 0
        });
    }

    function restoreSearchScrollPosition(oController) {
        var oScrollState = ModelStateRuntime.read(oController, STATE_MODEL, STATE_PATHS.SEARCH_SCROLL_STATE);
        var iTargetTop = Number(oScrollState && oScrollState.hostTop);
        if (!oScrollState) {
            return;
        }
        SchedulingRuntime.nextDoubleFrame(function () {
            var oScrollHost = resolveSearchScrollHost(oController);
            var iMaxTop;
            if (!oScrollHost || !Number.isFinite(iTargetTop)) {
                return;
            }
            iMaxTop = Math.max(0, oScrollHost.scrollHeight - oScrollHost.clientHeight);
            oScrollHost.scrollTop = Math.max(0, Math.min(iTargetTop, iMaxTop));
            ModelStateRuntime.write(oController, STATE_MODEL, STATE_PATHS.SEARCH_SCROLL_STATE, null);
            syncSearchViewportLayout(oController, false);
        });
    }

    function scrollToSearchFilters(oController) {
        var oFilterCard = oController.byId && oController.byId(UiControlIds.SEARCH.FILTER_CARD);
        var oFilterDom = oFilterCard && oFilterCard.getDomRef && oFilterCard.getDomRef();
        return scrollToTarget(oController, oFilterDom, resolveShellHeaderOffset(oController, resolveSearchScrollHost(oController)));
    }

    function scrollToSearchResultsToolbar(oController) {
        var oResultsShell = oController.byId && oController.byId(UiControlIds.SEARCH.RESULTS_SHELL);
        var oResultsShellDom = oResultsShell && oResultsShell.getDomRef && oResultsShell.getDomRef();
        var oResultsToolbarDom = resolveSearchTableToolbarDom(oController);
        var oToolbarDom = oResultsToolbarDom || oResultsShellDom;
        return scrollToTarget(oController, oToolbarDom, resolveShellHeaderOffset(oController, resolveSearchScrollHost(oController)));
    }

    function scrollToTarget(oController, oTargetDom, iStickyOffset) {
        var oScrollHost = resolveSearchScrollHost(oController);
        var oHostRect;
        var oTargetRect;
        var iTargetTop;
        if (!oScrollHost || !oTargetDom || !oScrollHost.getBoundingClientRect || !oTargetDom.getBoundingClientRect) {
            if (oTargetDom && oTargetDom.scrollIntoView) {
                oTargetDom.scrollIntoView({ block: "start", behavior: "smooth" });
            }
            return Promise.resolve(false);
        }
        oHostRect = oScrollHost.getBoundingClientRect();
        oTargetRect = oTargetDom.getBoundingClientRect();
        iTargetTop = (oScrollHost.scrollTop || 0) + (oTargetRect.top - oHostRect.top) - iStickyOffset - SEARCH_ANCHOR_SCROLL_MARGIN_PX;
        oScrollHost.scrollTop = Math.max(0, iTargetTop);
        syncSearchViewportLayout(oController, false);
        return Promise.resolve(true);
    }

    return {
        bindSearchViewportRuntime: bindSearchViewportRuntime,
        buildSearchViewportLayoutKey: buildSearchViewportLayoutKey,
        captureSearchScrollPosition: captureSearchScrollPosition,
        resolveSearchScrollHost: resolveSearchScrollHost,
        resolveSearchTableToolbarDom: resolveSearchTableToolbarDom,
        resolveSearchViewportObserverTargets: resolveSearchViewportObserverTargets,
        resolveShellHeaderOffset: resolveShellHeaderOffset,
        restoreSearchScrollPosition: restoreSearchScrollPosition,
        scheduleSearchViewportSync: scheduleSearchViewportSync,
        scrollToSearchFilters: scrollToSearchFilters,
        scrollToSearchResultsToolbar: scrollToSearchResultsToolbar,
        scrollToTarget: scrollToTarget,
        syncSearchStickyOffsets: syncSearchStickyOffsets,
        syncSearchViewportLayout: syncSearchViewportLayout,
        unbindSearchViewportRuntime: unbindSearchViewportRuntime
    };
});
