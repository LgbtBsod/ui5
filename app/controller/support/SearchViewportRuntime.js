sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/util/ThemeDomRuntime"
], function (SearchSelectionRuntime, ControllerModelRuntime, ControllerViewStateRuntime, ModelStateRuntime, SchedulingRuntime, ThemeDomRuntime) {
    "use strict";

    var SEARCH_VIEWPORT_LAYOUT_DEBOUNCE_MS = 96;

    function setSearchViewportCssVar(oController, sName, sValue) {
        var oViewDom = oController && oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
        ThemeDomRuntime.setStyleProperty([oViewDom], sName, sValue);
    }

    function resolveSearchScrollHost(oController) {
        var oDomRef = oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
        var aCandidates;
        var oNode = oDomRef && oDomRef.parentElement;
        var oDocumentScrollHost;
        if (oDomRef && oDomRef.querySelectorAll) {
            aCandidates = Array.prototype.slice.call(oDomRef.querySelectorAll(".sapMPageEnableScrolling, .sapMPageScroll, .sapMPageEnableScrolling > div"));
            oNode = aCandidates.find(function (oCandidate) {
                return oCandidate && oCandidate.scrollHeight > oCandidate.clientHeight + 4;
            }) || oNode;
            if (oNode && oNode.scrollHeight > oNode.clientHeight + 4) {
                return oNode;
            }
        }
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

    function setSearchStickyTop(vControlOrDom, sTop) {
        var oDomRef = vControlOrDom && vControlOrDom.nodeType ? vControlOrDom : (vControlOrDom && vControlOrDom.getDomRef && vControlOrDom.getDomRef());
        ThemeDomRuntime.setStyleProperty([oDomRef], "top", sTop);
    }

    function resolveShellHeaderOffset(oController, oScrollHost) {
        var oShellHeader = typeof document !== "undefined" ? document.querySelector(".appShellHeader") : null;
        var iShellBottom = 0;
        var iHostTop = 0;
        if (oShellHeader && oShellHeader.getBoundingClientRect) {
            iShellBottom = Math.ceil(oShellHeader.getBoundingClientRect().bottom || 0);
        }
        if (oScrollHost && oScrollHost.getBoundingClientRect) {
            iHostTop = Math.ceil(oScrollHost.getBoundingClientRect().top || 0);
        }
        return Math.max(8, iShellBottom - iHostTop + 2);
    }

    function clearSearchViewportSyncTimer(oController) {
        oController._iSearchViewportSyncTimer = SchedulingRuntime.clearTimer(oController._iSearchViewportSyncTimer);
    }

    function syncSearchViewportLayout(oController) {
        var oInnerTable = SearchSelectionRuntime.resolveSearchInnerTable(oController);
        SearchSelectionRuntime.configureSearchResultTable(oController, oInnerTable, false);
        syncSearchStickyOffsets(oController);
    }

    function syncSearchScrollAffordances(oController) {
        var oScrollHost = resolveSearchScrollHost(oController);
        var iTop = oScrollHost ? Number(oScrollHost.scrollTop || 0) : 0;
        var oResultsShell = oController.byId && oController.byId("searchResultsShell");
        var oResultsShellDom = oResultsShell && oResultsShell.getDomRef && oResultsShell.getDomRef();
        var oResultsToolbarDom = oResultsShellDom && oResultsShellDom.querySelector ? oResultsShellDom.querySelector(".sapUiCompSmartTableToolbar") : null;
        var oHostRect;
        var oToolbarRect;
        var iToolbarTop = 0;
        ControllerViewStateRuntime.set(oController, "/scrollNavVisible", iTop > 220);
        if (oScrollHost && oResultsToolbarDom && oScrollHost.getBoundingClientRect && oResultsToolbarDom.getBoundingClientRect) {
            oHostRect = oScrollHost.getBoundingClientRect();
            oToolbarRect = oResultsToolbarDom.getBoundingClientRect();
            iToolbarTop = iTop + (oToolbarRect.top - oHostRect.top);
        }
        ControllerViewStateRuntime.set(oController, "/resultsToolbarNavVisible", !!oResultsToolbarDom && iTop > (iToolbarTop + 120));
    }

    function flushSearchViewportSync(oController) {
        oController._iSearchViewportSyncRaf = SchedulingRuntime.requestFrameOnce(oController._iSearchViewportSyncRaf, function () {
            oController._iSearchViewportSyncRaf = 0;
            syncSearchViewportLayout(oController);
            syncSearchScrollAffordances(oController);
        });
    }

    function resolveSearchWorkbenchDock(oController) {
        return oController.byId && oController.byId("searchWorkbenchDock");
    }

    function resolveResultsTableToolbarHeight(oController) {
        return resolveDomHeight(resolveSearchTableToolbarDom(oController));
    }

    function syncSearchStickyOffsets(oController) {
        var oScrollHost = resolveSearchScrollHost(oController);
        var oWorkbenchDock = resolveSearchWorkbenchDock(oController);
        var oFilterCard = oController.byId && oController.byId("searchFilterCard");
        var oActionRail = oController.byId && oController.byId("searchResultsActionRail");
        var oToolbarRail = oController.byId && oController.byId("smartTableCustomToolbar");
        var oResultsToolbarDom = resolveSearchTableToolbarDom(oController);
        var iResultsToolbarHeight = resolveResultsTableToolbarHeight(oController);
        var iFilterHeight = resolveDomHeight(oFilterCard, ".searchFilterCardDense");
        var iActionHeight = resolveDomHeight(oActionRail, ".searchResultsActionRail");
        var iToolbarHeight = resolveDomHeight(oToolbarRail, ".searchSmartToolbarRail");
        var iDockHeight = resolveOuterHeight(oWorkbenchDock);
        var iTopBase = resolveShellHeaderOffset(oController, oScrollHost);
        var iStackGap = 6;
        var iResultsToolbarGap = iResultsToolbarHeight ? 8 : 0;
        var iActionTop;
        var iToolbarTop;
        var iTableToolbarTop;
        var iHeaderTop;
        if (!iDockHeight) {
            iDockHeight = iFilterHeight + iActionHeight + iToolbarHeight;
            if (iFilterHeight && iActionHeight) {
                iDockHeight += iStackGap;
            }
            if ((iFilterHeight || iActionHeight) && iToolbarHeight) {
                iDockHeight += iStackGap;
            }
        }
        iActionTop = iTopBase + iFilterHeight + (iFilterHeight && iActionHeight ? iStackGap : 0);
        iToolbarTop = iActionTop + iActionHeight + (iActionHeight && iToolbarHeight ? iStackGap : 0);
        iTableToolbarTop = iToolbarTop + iToolbarHeight + (iToolbarHeight && iResultsToolbarHeight ? iResultsToolbarGap : 8);
        iHeaderTop = iTableToolbarTop + iResultsToolbarHeight + iResultsToolbarGap;
        setSearchViewportCssVar(oController, "--search-sticky-dock-top", iTopBase + "px");
        setSearchViewportCssVar(
            oController,
            "--search-workbench-toolbar-stack-height",
            (iActionHeight + iToolbarHeight + ((iActionHeight && iToolbarHeight) ? iStackGap : 0)) + "px"
        );
        setSearchViewportCssVar(oController, "--search-sticky-filter-top", iTopBase + "px");
        setSearchViewportCssVar(oController, "--search-sticky-action-top", iActionTop + "px");
        setSearchViewportCssVar(oController, "--search-sticky-toolbar-top", iToolbarTop + "px");
        setSearchViewportCssVar(oController, "--search-sticky-table-toolbar-top", iTableToolbarTop + "px");
        setSearchViewportCssVar(oController, "--search-smarttable-toolbar-height", iResultsToolbarHeight + "px");
        setSearchViewportCssVar(oController, "--search-sticky-header-top", iHeaderTop + "px");
        setSearchStickyTop(oFilterCard, iTopBase + "px");
        setSearchStickyTop(oActionRail, iActionTop + "px");
        setSearchStickyTop(oToolbarRail, iToolbarTop + "px");
        setSearchStickyTop(oResultsToolbarDom, iTableToolbarTop + "px");
        ThemeDomRuntime.setStyleProperties([oActionRail, oToolbarRail, oResultsToolbarDom], {
            "overflow": "visible",
            "overflow-x": "visible",
            "overflow-y": "visible"
        }, "important");
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
            oResultsShell && oResultsShell.getDomRef && oResultsShell.getDomRef(),
            resolveSearchTableToolbarDom(oController)
        ].forEach(function (oTarget) {
            if (oTarget && aTargets.indexOf(oTarget) < 0) {
                aTargets.push(oTarget);
            }
        });
        return aTargets;
    }

    function bindSearchViewportObservers(oController, oScrollHost) {
        var aPrevTargets;
        var aNextTargets;
        if (typeof window === "undefined" || typeof window.ResizeObserver !== "function") {
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
            oView.addEventDelegate(oController._oSearchViewportDelegate);
        }
        if (oController._oSearchScrollHost === oScrollHost) {
            bindSearchViewportObservers(oController, oScrollHost);
            scheduleSearchViewportSync(oController, true);
            syncSearchScrollAffordances(oController);
            return;
        }
        if (oController._oSearchScrollHost && oController._fnSearchScrollSync) {
            oController._oSearchScrollHost.removeEventListener("scroll", oController._fnSearchScrollSync, { passive: true });
        }
        oController._oSearchScrollHost = oScrollHost;
        if (!oController._fnSearchScrollSync) {
            oController._fnSearchScrollSync = function () {
                syncSearchScrollAffordances(oController);
            };
        }
        if (!oController._fnSearchViewportResize) {
            oController._fnSearchViewportResize = function () {
                scheduleSearchViewportSync(oController, false);
            };
            window.addEventListener("resize", oController._fnSearchViewportResize);
        }
        if (oScrollHost) {
            oScrollHost.addEventListener("scroll", oController._fnSearchScrollSync, { passive: true });
        }
        bindSearchViewportObservers(oController, oScrollHost);
        scheduleSearchViewportSync(oController, true);
        syncSearchScrollAffordances(oController);
    }

    function unbindSearchViewportRuntime(oController) {
        var oView = oController && oController.getView && oController.getView();
        if (oView && oController._oSearchViewportDelegate && oView.removeEventDelegate) {
            oView.removeEventDelegate(oController._oSearchViewportDelegate);
        }
        if (oController._oSearchScrollHost && oController._fnSearchScrollSync) {
            oController._oSearchScrollHost.removeEventListener("scroll", oController._fnSearchScrollSync, { passive: true });
        }
        if (oController._fnSearchViewportResize) {
            window.removeEventListener("resize", oController._fnSearchViewportResize);
        }
        unbindSearchViewportObservers(oController);
        clearSearchViewportSyncTimer(oController);
        oController._iSearchViewportSyncRaf = SchedulingRuntime.clearFrame(oController._iSearchViewportSyncRaf);
        oController._oSearchViewportDelegate = null;
        oController._oSearchScrollHost = null;
        oController._fnSearchScrollSync = null;
        oController._fnSearchViewportResize = null;
        oController._sSearchTableLayoutKey = "";
        oController._iSearchViewportSyncTimer = 0;
        oController._iSearchViewportSyncRaf = 0;
    }

    function captureSearchScrollPosition(oController) {
        var oScrollHost = resolveSearchScrollHost(oController);
        if (!ControllerModelRuntime.state(oController)) {
            return;
        }
        ModelStateRuntime.write(oController, "state", "/searchScrollState", {
            hostTop: oScrollHost ? oScrollHost.scrollTop : 0
        });
    }

    function restoreSearchScrollPosition(oController) {
        var oScrollState = ModelStateRuntime.read(oController, "state", "/searchScrollState");
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
            ModelStateRuntime.write(oController, "state", "/searchScrollState", null);
            syncSearchViewportLayout(oController);
            syncSearchScrollAffordances(oController);
        });
    }

    function scrollToSearchFilters(oController) {
        var oScrollHost = resolveSearchScrollHost(oController);
        var oFilterCard = oController.byId && oController.byId("searchFilterCard");
        var oFilterDom = oFilterCard && oFilterCard.getDomRef && oFilterCard.getDomRef();
        var oHostRect;
        var oFilterRect;
        var iTargetTop;
        var iStickyOffset = resolveShellHeaderOffset(oController, oScrollHost);
        if (!oScrollHost || !oFilterDom || !oScrollHost.getBoundingClientRect || !oFilterDom.getBoundingClientRect) {
            if (oFilterDom && oFilterDom.scrollIntoView) {
                oFilterDom.scrollIntoView({ block: "start", behavior: "smooth" });
            }
            return Promise.resolve(false);
        }
        oHostRect = oScrollHost.getBoundingClientRect();
        oFilterRect = oFilterDom.getBoundingClientRect();
        iTargetTop = (oScrollHost.scrollTop || 0) + (oFilterRect.top - oHostRect.top) - iStickyOffset - 10;
        if (typeof oScrollHost.scrollTo === "function") {
            oScrollHost.scrollTo({ top: Math.max(0, iTargetTop), behavior: "smooth" });
        } else {
            oScrollHost.scrollTop = Math.max(0, iTargetTop);
        }
        syncSearchScrollAffordances(oController);
        return Promise.resolve(true);
    }

    function scrollToSearchResultsToolbar(oController) {
        var oScrollHost = resolveSearchScrollHost(oController);
        var oResultsShell = oController.byId && oController.byId("searchResultsShell");
        var oResultsShellDom = oResultsShell && oResultsShell.getDomRef && oResultsShell.getDomRef();
        var oFallbackToolbar = oController.byId && oController.byId("smartTableCustomToolbar");
        var oToolbarDom = oResultsShellDom && oResultsShellDom.querySelector ? oResultsShellDom.querySelector(".sapUiCompSmartTableToolbar") : null;
        var oHostRect;
        var oToolbarRect;
        var iTargetTop;
        var iStickyOffset = resolveShellHeaderOffset(oController, oScrollHost);
        if (!oToolbarDom) {
            oToolbarDom = oFallbackToolbar && oFallbackToolbar.getDomRef && oFallbackToolbar.getDomRef();
        }
        if (!oScrollHost || !oToolbarDom || !oScrollHost.getBoundingClientRect || !oToolbarDom.getBoundingClientRect) {
            if (oToolbarDom && oToolbarDom.scrollIntoView) {
                oToolbarDom.scrollIntoView({ block: "start", behavior: "smooth" });
            }
            return Promise.resolve(false);
        }
        oHostRect = oScrollHost.getBoundingClientRect();
        oToolbarRect = oToolbarDom.getBoundingClientRect();
        iTargetTop = (oScrollHost.scrollTop || 0) + (oToolbarRect.top - oHostRect.top) - iStickyOffset - 10;
        if (typeof oScrollHost.scrollTo === "function") {
            oScrollHost.scrollTo({ top: Math.max(0, iTargetTop), behavior: "smooth" });
        } else {
            oScrollHost.scrollTop = Math.max(0, iTargetTop);
        }
        syncSearchScrollAffordances(oController);
        return Promise.resolve(true);
    }

    return {
        bindSearchViewportRuntime: bindSearchViewportRuntime,
        captureSearchScrollPosition: captureSearchScrollPosition,
        restoreSearchScrollPosition: restoreSearchScrollPosition,
        scheduleSearchViewportSync: scheduleSearchViewportSync,
        scrollToSearchFilters: scrollToSearchFilters,
        scrollToSearchResultsToolbar: scrollToSearchResultsToolbar,
        syncSearchScrollAffordances: syncSearchScrollAffordances,
        syncSearchViewportLayout: syncSearchViewportLayout,
        unbindSearchViewportRuntime: unbindSearchViewportRuntime
    };
});
