sap.ui.define([
    "checklist/app/controller/support/SearchSelectionSupport",
    "checklist/app/controller/support/SearchLoadRuntimeSupport",
    "checklist/app/controller/support/SearchRateProgress",
    "checklist/app/controller/support/SearchCommandPolicy",
    "checklist/app/service/framework/FocusRuntime",
    "checklist/app/service/framework/ControlStyleRuntime",
    "checklist/app/service/framework/DialogOrchestrator",
    "checklist/app/service/framework/NavigationIntentService",
    "checklist/app/controller/base/ControllerTextRuntime",
    "checklist/app/service/framework/ControllerViewStateRuntime",
    "checklist/app/service/framework/ModelStateRuntime",
    "checklist/app/service/framework/SchedulingRuntime",
    "checklist/app/util/TimeConfigService",
    "checklist/app/util/ThemeDomRuntime",
    "checklist/app/controller/support/SearchViewStateSupport"
], function (SearchSelectionSupport, SearchLoadRuntimeSupport, SearchRateProgress, SearchCommandPolicy, FocusRuntime, ControlStyleRuntime, DialogOrchestrator, NavigationIntentService, ControllerTextRuntime, ControllerViewStateRuntime, ModelStateRuntime, SchedulingRuntime, TimeConfigService, ThemeDomRuntime, SearchViewStateSupport) {
    "use strict";

    var SEARCH_COLUMN_RULES = {
        Id: { width: "8.5rem", minScreenWidth: "", demandPopin: false, importance: "High" },
        LpcText: { width: "6.75rem", minScreenWidth: "Tablet", demandPopin: true, importance: "High" },
        ProfessionText: { width: "10rem", minScreenWidth: "Desktop", demandPopin: true, importance: "Medium" },
        LocationKey: { width: "9.75rem", minScreenWidth: "Desktop", demandPopin: true, importance: "Medium" },
        Status: { width: "7rem", minScreenWidth: "Tablet", demandPopin: true, importance: "High" },
        SuccessChecksRate: { width: "8rem", minScreenWidth: "Desktop", demandPopin: true, importance: "Medium" },
        SuccessBarriersRate: { width: "8rem", minScreenWidth: "Desktop", demandPopin: true, importance: "Medium" },
        DateCheck: { width: "8rem", minScreenWidth: "Tablet", demandPopin: true, importance: "High" },
        EquipName: { width: "9rem", minScreenWidth: "Desktop", demandPopin: true, importance: "Low" },
        ChangedOn: { width: "9rem", minScreenWidth: "Desktop", demandPopin: true, importance: "Low" }
    };
    var SEARCH_WORKING_HINT_MS = 2000;
    var SEARCH_INITIAL_ANALYTICS_DELAY_MS = 400;
    var SEARCH_VIEWPORT_LAYOUT_DEBOUNCE_MS = 96;
    var EFFECT_DIALOGS = {
        workflowAnalytics: "checklist.app.view.fragment.WorkflowAnalyticsDialog"
    };

    function resolveStartupPerf(oController) {
        var oOwner = oController && oController.getOwnerComponent && oController.getOwnerComponent();
        if (!oOwner) {
            return null;
        }
        oOwner._startupPerf = oOwner._startupPerf || {
            t0: (window.performance && typeof window.performance.now === "function") ? window.performance.now() : Date.now(),
            firstRouteReadyLogged: false,
            analyticsStartedLogged: false
        };
        return oOwner._startupPerf;
    }

    function nowMs() {
        return (window.performance && typeof window.performance.now === "function") ? window.performance.now() : Date.now();
    }

    function logStartupMetric(oController, sEvent) {
        var oPerf = resolveStartupPerf(oController);
        var iDelta;
        if (!oPerf || !oPerf.t0) {
            return;
        }
        if (sEvent === "firstRouteReady" && oPerf.firstRouteReadyLogged) {
            return;
        }
        if (sEvent === "analyticsStarted" && oPerf.analyticsStartedLogged) {
            return;
        }
        iDelta = Math.max(0, Math.round(nowMs() - oPerf.t0));
        if (sEvent === "firstRouteReady") {
            oPerf.firstRouteReadyLogged = true;
            console.info("[Startup] first route ready:", iDelta + "ms");
            return;
        }
        if (sEvent === "analyticsStarted") {
            oPerf.analyticsStartedLogged = true;
            console.info("[Startup] analytics started:", iDelta + "ms");
        }
    }

    function clearSearchWorkingHintTimer(oController) {
        oController._iSearchWorkingHintTimer = SchedulingRuntime.clearTimer(oController._iSearchWorkingHintTimer);
    }

    function hideSearchWorkingHint(oController) {
        clearSearchWorkingHintTimer(oController);
        ControllerViewStateRuntime.set(oController, "/filterHintVisible", false);
        ControllerViewStateRuntime.set(oController, "/filterHintText", "");
    }

    function isSearchLoading(oController) {
        return !!(
            ControllerViewStateRuntime.get(oController, "/tableBusy", false)
            || ControllerViewStateRuntime.get(oController, "/searchActionBusy", false)
            || ModelStateRuntime.read(oController, "state", "/isLoading", false)
        );
    }

    function scheduleSearchWorkingHint(oController) {
        clearSearchWorkingHintTimer(oController);
        oController._iSearchWorkingHintTimer = SchedulingRuntime.restartTimer(0, function () {
            if (!isSearchLoading(oController)) {
                return;
            }
            ControllerViewStateRuntime.set(oController, "/filterHintVisible", true);
            ControllerViewStateRuntime.set(oController, "/filterHintType", "Information");
            ControllerViewStateRuntime.set(
                oController,
                "/filterHintText",
                ControllerTextRuntime.getText(oController, "workingMessageLong", [], "Working...")
            );
        }, SEARCH_WORKING_HINT_MS);
    }

    function beginSearchLoadingFeedback(oController) {
        scheduleSearchWorkingHint(oController);
    }

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

    function resolveDescendantHeight(oControl, sSelector) {
        var oDomRef = oControl && oControl.getDomRef && oControl.getDomRef();
        var oTarget = oDomRef && oDomRef.querySelector ? oDomRef.querySelector(sSelector) : null;
        if (!oTarget || !oTarget.getBoundingClientRect) {
            return 0;
        }
        return Math.max(0, Math.ceil(oTarget.getBoundingClientRect().height || 0));
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
        var oResultsShell = oController.byId && oController.byId("searchResultsShell");
        return resolveDescendantHeight(oResultsShell, ".sapUiCompSmartTableToolbar");
    }

    function syncSearchStickyOffsets(oController) {
        var oScrollHost = resolveSearchScrollHost(oController);
        var oWorkbenchDock = resolveSearchWorkbenchDock(oController);
        var oFilterCard = oController.byId && oController.byId("searchFilterCard");
        var oActionRail = oController.byId && oController.byId("searchResultsActionRail");
        var oToolbarRail = oController.byId && oController.byId("smartTableCustomToolbar");
        var iResultsToolbarHeight = resolveResultsTableToolbarHeight(oController);
        var iFilterHeight = resolveOuterHeight(oFilterCard);
        var iActionHeight = resolveOuterHeight(oActionRail);
        var iToolbarHeight = resolveOuterHeight(oToolbarRail);
        var iDockHeight = resolveOuterHeight(oWorkbenchDock);
        var iTopBase = resolveShellHeaderOffset(oController, oScrollHost);
        var iStackGap = 6;
        var iResultsToolbarGap = iResultsToolbarHeight ? 8 : 0;
        if (!iDockHeight) {
            iDockHeight = iFilterHeight + iActionHeight + iToolbarHeight;
            if (iFilterHeight && iActionHeight) {
                iDockHeight += iStackGap;
            }
            if ((iFilterHeight || iActionHeight) && iToolbarHeight) {
                iDockHeight += iStackGap;
            }
        }
        var iTableToolbarTop = iTopBase + iDockHeight + 8;
        var iHeaderTop = iTableToolbarTop + iResultsToolbarHeight + iResultsToolbarGap;
        setSearchViewportCssVar(oController, "--search-sticky-dock-top", iTopBase + "px");
        setSearchViewportCssVar(
            oController,
            "--search-workbench-toolbar-stack-height",
            (iActionHeight + iToolbarHeight + ((iActionHeight && iToolbarHeight) ? iStackGap : 0)) + "px"
        );
        setSearchViewportCssVar(oController, "--search-sticky-filter-top", iTopBase + "px");
        setSearchViewportCssVar(oController, "--search-sticky-action-top", iTopBase + "px");
        setSearchViewportCssVar(oController, "--search-sticky-toolbar-top", iTopBase + "px");
        setSearchViewportCssVar(oController, "--search-sticky-table-toolbar-top", iTableToolbarTop + "px");
        setSearchViewportCssVar(oController, "--search-smarttable-toolbar-height", iResultsToolbarHeight + "px");
        setSearchViewportCssVar(oController, "--search-sticky-header-top", iHeaderTop + "px");
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

    function syncSearchViewportLayout(oController) {
        var oSmartTable = oController.byId && oController.byId("searchSmartTable");
        var oInnerTable = oSmartTable && oSmartTable.getTable && oSmartTable.getTable();
        configureSearchResultTable(oController, oInnerTable, false);
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

    function resolveSearchViewportObserverTargets(oController, oScrollHost) {
        var aTargets = [];
        var oViewDom = oController && oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
        var oWorkbenchDock = resolveSearchWorkbenchDock(oController);
        var oWorkbenchDom = oWorkbenchDock && oWorkbenchDock.getDomRef && oWorkbenchDock.getDomRef();
        var oShellHeader = typeof document !== "undefined" ? document.querySelector(".appShellHeader") : null;
        [oViewDom, oScrollHost, oWorkbenchDom, oShellHeader].forEach(function (oTarget) {
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
        if (!oController.getModel("state")) {
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
            oScrollHost.scrollTo({
                top: Math.max(0, iTargetTop),
                behavior: "smooth"
            });
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
            oScrollHost.scrollTo({
                top: Math.max(0, iTargetTop),
                behavior: "smooth"
            });
        } else {
            oScrollHost.scrollTop = Math.max(0, iTargetTop);
        }
        syncSearchScrollAffordances(oController);
        return Promise.resolve(true);
    }

    function parseColumnPersonalizationData(oColumn) {
        var vData = oColumn && oColumn.data && oColumn.data("p13nData");
        if (!vData) {
            return null;
        }
        if (typeof vData === "string") {
            try {
                return JSON.parse(vData);
            } catch (oError) {
                return null;
            }
        }
        return typeof vData === "object" ? vData : null;
    }

    function resolveSearchColumnKey(oColumn) {
        var oP13nData = parseColumnPersonalizationData(oColumn) || {};
        var sKey = oP13nData.columnKey || oP13nData.leadingProperty || oP13nData.sortProperty || oP13nData.filterProperty || "";
        var oHeader = oColumn && oColumn.getHeader && oColumn.getHeader();
        var sHeaderText = oHeader && oHeader.getText && oHeader.getText();
        if (sKey) {
            return String(sKey);
        }
        return String(sHeaderText || "");
    }

    function isCompactSearchViewport(oController) {
        var iRootSize = parseFloat(window.getComputedStyle(document.documentElement).fontSize || "16");
        var iViewportRem = resolveSearchViewportWidth(oController) / (Number.isFinite(iRootSize) && iRootSize > 0 ? iRootSize : 16);
        return iViewportRem <= 45;
    }

    function resolveSearchViewportWidth(oController) {
        var oSearchHost = oController && oController.byId && oController.byId("searchPaneHost");
        var oSearchHostDom = oSearchHost && oSearchHost.getDomRef && oSearchHost.getDomRef();
        var oResultsShell = oController && oController.byId && oController.byId("searchResultsShell");
        var oResultsShellDom = oResultsShell && oResultsShell.getDomRef && oResultsShell.getDomRef();
        var iWidth = 0;
        if (oResultsShellDom && oResultsShellDom.getBoundingClientRect) {
            iWidth = Math.floor(oResultsShellDom.getBoundingClientRect().width || 0);
        }
        if (!iWidth && oSearchHostDom && oSearchHostDom.getBoundingClientRect) {
            iWidth = Math.floor(oSearchHostDom.getBoundingClientRect().width || 0);
        }
        if (!iWidth && typeof window !== "undefined") {
            iWidth = Math.floor(window.innerWidth || 0);
        }
        return iWidth || 0;
    }

    function applySearchColumnRule(oController, oColumn, mRule, sColumnKey) {
        var bCompactViewport = isCompactSearchViewport(oController);
        var iViewportWidth = resolveSearchViewportWidth(oController);
        var bBaseVisible;
        if (!oColumn || !mRule) {
            return;
        }
        if (typeof oColumn.data === "function" && typeof oColumn.data("chkBaseVisible") !== "boolean") {
            oColumn.data("chkBaseVisible", !(typeof oColumn.getVisible === "function") || oColumn.getVisible());
        }
        bBaseVisible = typeof oColumn.data === "function" && typeof oColumn.data("chkBaseVisible") === "boolean"
            ? oColumn.data("chkBaseVisible")
            : true;
        if (typeof oColumn.setWidth === "function") {
            oColumn.setWidth(bCompactViewport ? "auto" : (mRule.width || "auto"));
        }
        if (typeof oColumn.setMinScreenWidth === "function") {
            oColumn.setMinScreenWidth(mRule.minScreenWidth || "");
        }
        if (typeof oColumn.setDemandPopin === "function") {
            oColumn.setDemandPopin(!!mRule.demandPopin);
        }
        if (typeof oColumn.setImportance === "function" && mRule.importance) {
            oColumn.setImportance(mRule.importance);
        }
        if (typeof oColumn.setPopinDisplay === "function") {
            oColumn.setPopinDisplay(bCompactViewport ? "Block" : "Inline");
        }
        if (typeof oColumn.setVisible === "function") {
            oColumn.setVisible(!!bBaseVisible);
        }
        if (typeof oColumn.setHAlign === "function" && (sColumnKey === "SuccessChecksRate" || sColumnKey === "SuccessBarriersRate")) {
            oColumn.setHAlign("Center");
        }
        if (typeof oColumn.toggleStyleClass === "function") {
            oColumn.toggleStyleClass("searchColumnCritical", mRule.importance === "High");
            oColumn.toggleStyleClass("searchColumnSecondary", mRule.importance === "Low");
            oColumn.toggleStyleClass("searchColumnHiddenNarrow", false);
        }
    }

    function configureSearchResultTable(oController, oInnerTable, bForce) {
        var aColumns;
        var bCompactViewport = isCompactSearchViewport(oController);
        var iViewportWidth = resolveSearchViewportWidth(oController);
        var sTableId;
        var sLayoutKey;
        if (!oInnerTable) {
            return;
        }
        sTableId = oInnerTable && oInnerTable.getId ? oInnerTable.getId() : "searchInnerTable";
        sLayoutKey = [sTableId, bCompactViewport ? "compact" : "regular", iViewportWidth].join("::");
        if (!bForce && oController._sSearchTableLayoutKey === sLayoutKey) {
            return;
        }
        if (typeof oInnerTable.setFixedLayout === "function") {
            oInnerTable.setFixedLayout(bCompactViewport);
        }
        if (typeof oInnerTable.setAutoPopinMode === "function") {
            oInnerTable.setAutoPopinMode(false);
        }
        aColumns = oInnerTable.getColumns ? (oInnerTable.getColumns() || []) : [];
        aColumns.forEach(function (oColumn) {
            var sColumnKey = resolveSearchColumnKey(oColumn);
            applySearchColumnRule(oController, oColumn, SEARCH_COLUMN_RULES[sColumnKey], sColumnKey);
        });
        oController._sSearchTableLayoutKey = sLayoutKey;
    }

    function getWorkflowAnalyticsDialog(oController) {
        return DialogOrchestrator.ensure(oController, "workflowAnalytics", {
            fragmentName: "checklist.app.view.fragment.WorkflowAnalyticsDialog",
            afterOpen: function (oDialog, oCtrl) {
                if (oDialog && oDialog.data && !oDialog.data("workflowAnalyticsFocusBound")) {
                    oDialog.data("workflowAnalyticsFocusBound", true);
                }
                FocusRuntime.focusSoon(oCtrl && oCtrl.byId && oCtrl.byId("workflowAnalyticsCloseButton"));
            },
            afterClose: function (_oDialog, oCtrl) {
                if (oCtrl && typeof oCtrl._restoreWorkflowAnalyticsFocus === "function") {
                    oCtrl._restoreWorkflowAnalyticsFocus();
                }
            }
        });
    }

    function ensureEffectDialog(oController, sId) {
        return EFFECT_DIALOGS[sId] ? getWorkflowAnalyticsDialog(oController) : Promise.resolve(null);
    }

    function shouldAllowDialogEffect(oController, sId, sAction) {
        if (sId !== "workflowAnalytics" || sAction !== "open") {
            return true;
        }
        if (!oController._bWorkflowAnalyticsOpenRequested) {
            return false;
        }
        oController._bWorkflowAnalyticsOpenRequested = false;
        return true;
    }

    function closeWorkflowAnalyticsIfOpen(oController) {
        var oDialog = oController.byId("workflowAnalyticsDialog");
        oController._bWorkflowAnalyticsOpenRequested = false;
        if (oDialog && oDialog.isOpen && oDialog.isOpen() && oDialog.close) {
            oDialog.close();
        }
    }

    function resolveSmartSearchButton(oController) {
        var oSmartFilterBar = oController.byId("searchSmartFilterBar");
        var aButtons;
        if (!oSmartFilterBar || typeof oSmartFilterBar.findAggregatedObjects !== "function") {
            return null;
        }
        aButtons = oSmartFilterBar.findAggregatedObjects(true, function (oCandidate) {
            var sName = oCandidate && oCandidate.getMetadata && oCandidate.getMetadata().getName();
            if (sName !== "sap.m.Button") {
                return false;
            }
            if (typeof oCandidate.getType === "function" && oCandidate.getType() === "Emphasized") {
                return true;
            }
            return false;
        }) || [];
        return aButtons[0] || null;
    }

    function resolveSearchInnerTable(oController) {
        var oSmartTable = oController.byId("searchSmartTable");
        return oSmartTable && oSmartTable.getTable && oSmartTable.getTable();
    }

    function normalizeChecklistIds(aIds) {
        var mSeen = {};
        return (aIds || []).reduce(function (aAcc, sId) {
            var sNorm = String(sId || "").trim();
            if (!sNorm || mSeen[sNorm]) {
                return aAcc;
            }
            mSeen[sNorm] = true;
            aAcc.push(sNorm);
            return aAcc;
        }, []);
    }

    function extractChecklistIdFromListItem(oListItem) {
        var oCtx = oListItem && oListItem.getBindingContext && oListItem.getBindingContext();
        var oObject = oCtx && oCtx.getObject && oCtx.getObject();
        return SearchSelectionSupport.extractChecklistIdFromObject(oObject);
    }

    function resolveSelectedRowIdsFromInnerTable(oInnerTable) {
        var aSelectedItems = oInnerTable && oInnerTable.getSelectedItems ? (oInnerTable.getSelectedItems() || []) : [];
        return normalizeChecklistIds(aSelectedItems.map(extractChecklistIdFromListItem));
    }

    function applySelectionState(oController, aSelectedRowIds, sSource) {
        var aIds = normalizeChecklistIds(aSelectedRowIds);
        return SearchCommandPolicy.selectionChanged(oController, {
            selectedRowId: aIds[0] || "",
            selectedRowIds: aIds,
            source: sSource || "selectionRuntime"
        });
    }

    function focusDomNode(oNode) {
        if (!oNode || typeof oNode.focus !== "function") {
            return false;
        }
        try {
            if (typeof oNode.getAttribute === "function" && !oNode.getAttribute("tabindex")) {
                oNode.setAttribute("tabindex", "-1");
            }
        } catch (_e) {
            // Ignore readonly attribute nodes.
        }
        SchedulingRuntime.restartTimer(0, function () {
            oNode.focus();
        }, 0);
        return true;
    }

    function focusDomSelector(sSelector) {
        if (typeof document === "undefined" || !sSelector) {
            return false;
        }
        return focusDomNode(document.querySelector(sSelector));
    }

    function focusSearchFilters(oController) {
        var oTarget = resolveSmartSearchButton(oController) || oController.byId("searchSmartFilterBar");
        if (!oTarget) {
            return focusDomSelector("[id$='searchSmartFilterBar-btnGo']")
                || focusDomSelector("[id$='searchSmartFilterBar']")
                || focusDomSelector("[id$='searchSmartFilterBar'] input");
        }
        if (FocusRuntime.focusSoon(oTarget)) {
            return true;
        }
        return focusDomSelector("[id$='searchSmartFilterBar-btnGo']")
            || focusDomSelector("[id$='searchSmartFilterBar']")
            || focusDomSelector("[id$='searchSmartFilterBar'] input");
    }

    function focusSearchResultsTable(oController) {
        var oInnerTable = resolveSearchInnerTable(oController);
        var aSelectedItems;
        var aItems;
        var oTarget;
        if (!oInnerTable) {
            return focusDomSelector("[id$='searchSmartTable']")
                || focusDomSelector(".searchResultsTable");
        }
        aSelectedItems = oInnerTable.getSelectedItems ? (oInnerTable.getSelectedItems() || []) : [];
        if (Array.isArray(aSelectedItems) && aSelectedItems.length) {
            oTarget = aSelectedItems[0];
        }
        if (!oTarget && oInnerTable.getItems) {
            aItems = oInnerTable.getItems() || [];
            if (Array.isArray(aItems) && aItems.length) {
                oTarget = aItems[0];
            }
        }
        if (!oTarget) {
            oTarget = oInnerTable;
        }
        if (FocusRuntime.focusSoon(oTarget)) {
            return true;
        }
        if (oTarget && typeof oTarget.getDomRef === "function" && focusDomNode(oTarget.getDomRef())) {
            return true;
        }
        return focusDomSelector("[id$='searchSmartTable']")
            || focusDomSelector(".searchResultsTable .sapMListTblRow")
            || focusDomSelector(".searchResultsTable .sapMListTbl");
    }

    function focusSearchToolbar(oController) {
        var oTarget = oController.byId("backendTopInput")
            || oController.byId("maxRowsInput")
            || oController.byId("smartTableCustomToolbar");
        if (!oTarget) {
            return focusDomSelector("[id$='backendTopInput-inner']")
                || focusDomSelector("[id$='maxRowsInput-inner']")
                || focusDomSelector("[id$='smartTableCustomToolbar']");
        }
        if (FocusRuntime.focusSoon(oTarget)) {
            return true;
        }
        if (oTarget && typeof oTarget.getDomRef === "function" && focusDomNode(oTarget.getDomRef())) {
            return true;
        }
        return focusDomSelector("[id$='backendTopInput-inner']")
            || focusDomSelector("[id$='backendTopInput']")
            || focusDomSelector("[id$='maxRowsInput-inner']")
            || focusDomSelector("[id$='maxRowsInput']")
            || focusDomSelector("[id$='smartTableCustomToolbar']")
            || focusDomSelector(".searchCreateActionBtn");
    }

    function selectVisibleRows(oController) {
        var oInnerTable = resolveSearchInnerTable(oController);
        var aItems = [];
        var aSelectedRowIds = [];
        if (!oInnerTable) {
            return Promise.resolve({ count: 0, selectedRowIds: [] });
        }
        aItems = oInnerTable.getItems ? (oInnerTable.getItems() || []) : [];
        if (oInnerTable.removeSelections) {
            oInnerTable.removeSelections(true);
        }
        if (oInnerTable.selectAll) {
            oInnerTable.selectAll();
        } else if (oInnerTable.setSelectedItem) {
            aItems.forEach(function (oItem) {
                oInnerTable.setSelectedItem(oItem, true);
            });
        }
        aSelectedRowIds = resolveSelectedRowIdsFromInnerTable(oInnerTable);
        return Promise.resolve(applySelectionState(oController, aSelectedRowIds, "selectVisibleRows")).then(function () {
            return {
                count: aSelectedRowIds.length,
                selectedRowIds: aSelectedRowIds
            };
        });
    }

    function clearSelection(oController) {
        var oInnerTable = resolveSearchInnerTable(oController);
        if (oInnerTable && oInnerTable.removeSelections) {
            oInnerTable.removeSelections(true);
        }
        return Promise.resolve(applySelectionState(oController, [], "clearSelection"));
    }

    function isEditableTarget(oTarget) {
        var sTagName;
        if (!oTarget) {
            return false;
        }
        sTagName = String(oTarget.tagName || "").toUpperCase();
        if (sTagName === "INPUT" || sTagName === "TEXTAREA" || sTagName === "SELECT") {
            return true;
        }
        if (oTarget.isContentEditable) {
            return true;
        }
        return !!(oTarget.closest && oTarget.closest("[contenteditable='true']"));
    }

    function isSearchKeyboardContext(oController, oEvent) {
        var oViewDom = oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
        var oTarget = oEvent && oEvent.target;
        var oActive;
        var sHash = typeof window !== "undefined" ? String(window.location.hash || "") : "";
        var bSearchRoute = sHash === "" || sHash === "#" || sHash.indexOf("/search") >= 0;
        var bViewAttached = !!(oViewDom && typeof document !== "undefined" && document.body && document.body.contains(oViewDom));
        if (!bViewAttached) {
            return false;
        }
        if (oTarget && oViewDom.contains(oTarget)) {
            return true;
        }
        oActive = typeof document !== "undefined" ? document.activeElement : null;
        if (oActive && oViewDom.contains(oActive)) {
            return true;
        }
        if (bSearchRoute) {
            return true;
        }
        return false;
    }

    function resolveShortcutAction(oEvent) {
        var sKey = String((oEvent && oEvent.key) || "").toLowerCase();
        var bAccel = !!(oEvent && (oEvent.ctrlKey || oEvent.metaKey));
        var bShift = !!(oEvent && oEvent.shiftKey);
        var bAlt = !!(oEvent && oEvent.altKey);
        if (!oEvent || oEvent.repeat) {
            return "";
        }
        if ((bAccel && bShift && !bAlt) || (bAccel && bAlt && !bShift)) {
            if (sKey === "n") {
                return "create";
            }
            if (sKey === "o") {
                return "openSelected";
            }
            if (sKey === "c") {
                return "copy";
            }
            if (sKey === "a") {
                return "selectVisible";
            }
            if (sKey === "l") {
                return "clearSelection";
            }
            if (sKey === "s") {
                return "search";
            }
            if (sKey === "e") {
                return "export";
            }
            if (sKey === "f") {
                return "focusFilters";
            }
            return "";
        }
        if (!bAccel && !bShift && bAlt) {
            if (sKey === "1") {
                return "focusFilters";
            }
            if (sKey === "2") {
                return "focusResults";
            }
            if (sKey === "3") {
                return "focusToolbar";
            }
        }
        return "";
    }

    function runShortcutAction(oController, sAction) {
        if (!sAction) {
            return false;
        }
        if (sAction === "create" && typeof oController.onCreate === "function") {
            oController.onCreate();
            return true;
        }
        if (sAction === "openSelected" && typeof oController.onOpenSelected === "function") {
            oController.onOpenSelected();
            return true;
        }
        if (sAction === "copy" && typeof oController.onCopy === "function") {
            oController.onCopy();
            return true;
        }
        if (sAction === "selectVisible" && typeof oController.onSelectVisibleRows === "function") {
            oController.onSelectVisibleRows();
            return true;
        }
        if (sAction === "clearSelection" && typeof oController.onClearSelection === "function") {
            oController.onClearSelection();
            return true;
        }
        if (sAction === "search" && typeof oController.onSmartSearch === "function") {
            oController.onSmartSearch();
            return true;
        }
        if (sAction === "export" && typeof oController.onExportMenuDefault === "function") {
            oController.onExportMenuDefault();
            return true;
        }
        if (sAction === "focusFilters") {
            return focusSearchFilters(oController);
        }
        if (sAction === "focusResults") {
            return focusSearchResultsTable(oController);
        }
        if (sAction === "focusToolbar") {
            return focusSearchToolbar(oController);
        }
        return false;
    }

    function handlePowerUserShortcut(oController, oEvent) {
        var sAction;
        if (!isSearchKeyboardContext(oController, oEvent)) {
            return;
        }
        if (isEditableTarget(oEvent.target) && !(oEvent.altKey || ((oEvent.ctrlKey || oEvent.metaKey) && oEvent.shiftKey))) {
            return;
        }
        sAction = resolveShortcutAction(oEvent);
        if (!sAction) {
            return;
        }
        if (runShortcutAction(oController, sAction)) {
            oEvent.preventDefault();
            oEvent.stopPropagation();
        }
    }

    function bindPowerUserShortcuts(oController) {
        if (typeof document === "undefined" || oController._fnSearchPowerUserShortcut) {
            return;
        }
        oController._fnSearchPowerUserShortcut = function (oEvent) {
            handlePowerUserShortcut(oController, oEvent);
        };
        document.addEventListener("keydown", oController._fnSearchPowerUserShortcut, true);
    }

    function unbindPowerUserShortcuts(oController) {
        if (typeof document === "undefined" || !oController._fnSearchPowerUserShortcut) {
            return;
        }
        document.removeEventListener("keydown", oController._fnSearchPowerUserShortcut, true);
        oController._fnSearchPowerUserShortcut = null;
    }

    function setSearchActionBusy(oController, bBusy) {
        var oSearchButton = resolveSmartSearchButton(oController);
        if (!oSearchButton) {
            return;
        }
        ControlStyleRuntime.enable(oSearchButton, "searchGoActionBtn");
        if (typeof oSearchButton.setBusy === "function") {
            oSearchButton.setBusy(!!bBusy);
            oSearchButton.setBusyIndicatorDelay(0);
        }
        if (typeof oSearchButton.setEnabled === "function") {
            oSearchButton.setEnabled(!bBusy);
        }
    }

    function clearAnalyticsRefreshTimer(oController) {
        oController._iAnalyticsRefreshTimer = SchedulingRuntime.clearTimer(oController._iAnalyticsRefreshTimer);
    }

    function clearInitialAnalyticsSchedule(oController) {
        oController._iInitialAnalyticsTimer = SchedulingRuntime.clearTimer(oController._iInitialAnalyticsTimer);
        if (oController._iInitialAnalyticsIdleId && window.cancelIdleCallback) {
            window.cancelIdleCallback(oController._iInitialAnalyticsIdleId);
            oController._iInitialAnalyticsIdleId = null;
        }
    }

    function resolveAnalyticsRefreshMs(oController) {
        var oStateModel = oController.getModel("state");
        var iMs = Number(TimeConfigService.read(oStateModel, "analyticsRefreshMs"));
        return Number.isFinite(iMs) && iMs >= 1000 ? iMs : 300000;
    }

    function pulseAnalyticsRailUpdate(oController) {
        var oRail = oController.byId("searchAnalyticsRail");
        if (!oRail) {
            return;
        }
        oController._iAnalyticsRailPulseTimer = SchedulingRuntime.clearTimer(oController._iAnalyticsRailPulseTimer);
        ControlStyleRuntime.restart(oRail, "searchAnalyticsRailPulse");
        oController._iAnalyticsRailPulseTimer = SchedulingRuntime.restartTimer(0, function () {
            ControlStyleRuntime.disable(oRail, "searchAnalyticsRailPulse");
            oController._iAnalyticsRailPulseTimer = null;
        }, 520);
    }

    function refreshAnalyticsRail(oController, mOptions) {
        var bSilent = !!(mOptions && mOptions.silent);
        if (!bSilent) {
            ControllerViewStateRuntime.set(oController, "/analyticsRailBusy", true);
            ControllerViewStateRuntime.set(oController, "/analyticsError", "");
        }
        return SearchCommandPolicy.analytics(oController, { intent: "refreshRail", silent: bSilent }).then(function (vResult) {
            if (bSilent) {
                pulseAnalyticsRailUpdate(oController);
            }
            return vResult;
        });
    }

    function scheduleAnalyticsRefresh(oController) {
        clearAnalyticsRefreshTimer(oController);
        oController._iAnalyticsRefreshTimer = SchedulingRuntime.restartTimer(0, function () {
            refreshAnalyticsRail(oController, { silent: true });
            scheduleAnalyticsRefresh(oController);
        }, resolveAnalyticsRefreshMs(oController));
    }

    function bindAnalyticsRefreshTimer(oController) {
        var oStateModel = oController.getModel("state");
        if (!oStateModel || oController._oAnalyticsRefreshBinding) {
            return;
        }
        if (!oController._fnAnalyticsRefreshChanged) {
            oController._fnAnalyticsRefreshChanged = function () {
                scheduleAnalyticsRefresh(oController);
            };
        }
        oController._oAnalyticsRefreshBinding = oStateModel.bindProperty("/timers/analyticsRefreshMs");
        oController._oAnalyticsRefreshBinding.attachChange(oController._fnAnalyticsRefreshChanged);
    }

    function syncSmartControlAvailability(oController) {
        ControllerViewStateRuntime.set(oController, "/tableBusy", false);
    }

    function shouldRefreshSearchOnReturn(oController) {
        return !!ModelStateRuntime.read(oController, "state", "/searchForceRefreshOnReturn", false) &&
            !!ControllerViewStateRuntime.get(oController, "/hasSearched", false);
    }

    function clearSearchRefreshFlag(oController) {
        ModelStateRuntime.write(oController, "state", "/searchForceRefreshOnReturn", false);
    }

    function refreshSearchTableIfNeeded(oController, sSource) {
        if (!shouldRefreshSearchOnReturn(oController) || !ControllerViewStateRuntime.get(oController, "/smartTableReady", false)) {
            return;
        }
        clearSearchRefreshFlag(oController);
        SearchCommandPolicy.rebind(oController, { source: sSource || "searchReturn" });
    }

    function onSearchMatched(oController) {
        closeWorkflowAnalyticsIfOpen(oController);
        syncSmartControlAvailability(oController);
        bindSearchViewportRuntime(oController);
        logStartupMetric(oController, "firstRouteReady");
        ControllerViewStateRuntime.set(oController, "/bootstrapBusy", true);
        ControllerViewStateRuntime.set(oController, "/analyticsBusy", false);
        ControllerViewStateRuntime.set(oController, "/analyticsRailBusy", true);
        ControllerViewStateRuntime.set(oController, "/analyticsError", "");
        clearInitialAnalyticsSchedule(oController);
        Promise.resolve(SearchCommandPolicy.bootstrap(oController, { reason: "routeMatched" }))
            .catch(function () {
                return null;
            })
            .finally(function () {
                var fnStartAnalytics = function () {
                    oController._iInitialAnalyticsIdleId = null;
                    oController._iInitialAnalyticsTimer = null;
                    logStartupMetric(oController, "analyticsStarted");
                    refreshAnalyticsRail(oController, { silent: false });
                    scheduleAnalyticsRefresh(oController);
                };
                if (window.requestIdleCallback) {
                    oController._iInitialAnalyticsIdleId = window.requestIdleCallback(fnStartAnalytics, { timeout: 800 });
                    return;
                }
                oController._iInitialAnalyticsTimer = SchedulingRuntime.restartTimer(0, fnStartAnalytics, SEARCH_INITIAL_ANALYTICS_DELAY_MS);
            });
        restoreSearchScrollPosition(oController);
        refreshSearchTableIfNeeded(oController, "routeMatchedReturn");
    }

    function onSmartTableInitialise(oController) {
        var oSmartTable = oController.byId("searchSmartTable");
        var oInnerTable = oSmartTable && oSmartTable.getTable && oSmartTable.getTable();
        ControllerViewStateRuntime.set(oController, "/smartTableReady", true);
        if (!oInnerTable) {
            return;
        }
        configureSearchResultTable(oController, oInnerTable, true);
        if (oInnerTable.setMode) {
            oInnerTable.setMode("MultiSelect");
        }
        if (oInnerTable.setIncludeItemInSelection) {
            oInnerTable.setIncludeItemInSelection(false);
        }
        if (oInnerTable.removeSelections) {
            oInnerTable.removeSelections(true);
        }
        if (oInnerTable.attachSelectionChange) {
            oInnerTable.attachSelectionChange(oController.onSearchTableSelectionChange, oController);
        }
        if (oInnerTable.attachItemPress) {
            oInnerTable.attachItemPress(oController.onSearchTableItemPress, oController);
        }
        SearchViewStateSupport.syncSearchTableRequestWindow(oController);
        SearchRateProgress.wireTable(oController, oInnerTable);
        bindSearchViewportRuntime(oController);
        scheduleSearchViewportSync(oController, true);
        refreshSearchTableIfNeeded(oController, "smartTableInitialise");
    }

    function onBeforeSmartTableRebind(oController, oEvent) {
        var oBindingParams = oEvent && oEvent.getParameter && oEvent.getParameter("bindingParams");
        var oStateModel = oController.getModel("state");
        var oSmartTable = oController.byId("searchSmartTable");
        var oInnerTable = oSmartTable && oSmartTable.getTable && oSmartTable.getTable();
        configureSearchResultTable(oController, oInnerTable, true);
        SearchViewStateSupport.syncSearchTableRequestWindow(oController);
        ControllerViewStateRuntime.set(oController, "/tableBusy", true);
        scheduleSearchWorkingHint(oController);
        scheduleSearchViewportSync(oController, false);
        SearchCommandPolicy.applyRebindPolicy(oController, {
            source: "beforeRebind",
            bindingParams: oBindingParams || {},
            state: (oStateModel && oStateModel.getData && oStateModel.getData()) || {},
            onDataReceived: function (oDataEvent) {
                var aRows = [];
                var oError = oDataEvent && oDataEvent.getParameter && (oDataEvent.getParameter("error") || oDataEvent.getParameter("data") && oDataEvent.getParameter("data").error);
                var sErrorMessage = String((oError && (oError.message || oError.statusText)) || "").trim();
                var oCtx = oController._ctx && oController._ctx();
                hideSearchWorkingHint(oController);
                if (oError) {
                    SearchLoadRuntimeSupport.applyLoadError(oController, sErrorMessage);
                    return;
                }
                if (oCtx && oCtx.smartControls && oCtx.smartControls.getVisibleRows) {
                    aRows = oCtx.smartControls.getVisibleRows() || [];
                }
                SearchLoadRuntimeSupport.applyLoadSuccess(oController, aRows);
                bindSearchViewportRuntime(oController);
                scheduleSearchViewportSync(oController, true);
            }
        }).catch(function (oError) {
            hideSearchWorkingHint(oController);
            SearchLoadRuntimeSupport.applyLoadError(oController, String((oError && oError.message) || "Search request failed"));
            return Promise.reject(oError);
        });
    }

    function openWorkflowAnalytics(oController) {
        oController._bWorkflowAnalyticsOpenRequested = false;
        NavigationIntentService.navigateToAnalytics(oController);
        return Promise.resolve();
    }

    function closeWorkflowAnalytics(oController) {
        oController._bWorkflowAnalyticsOpenRequested = false;
        NavigationIntentService.navigateBackFromAnalytics(oController);
    }

    function runExport(oController, sEntity) {
        var aSelectedRowIds = ControllerViewStateRuntime.get(oController, "/selectedRowIds", []) || [];
        var iBackendTop = Number(ModelStateRuntime.read(oController, "state", "/searchBackendTop", 0)) || 0;
        ControllerViewStateRuntime.set(oController, "/exportBusy", true);
        return SearchCommandPolicy.exportFlow(oController, {
            entity: sEntity || "screen",
            selectedRowIds: Array.isArray(aSelectedRowIds) ? aSelectedRowIds.slice(0) : [],
            backendTop: iBackendTop
        }).finally(function () {
            ControllerViewStateRuntime.set(oController, "/exportBusy", false);
        });
    }

    return {
        bindAnalyticsRefreshTimer: bindAnalyticsRefreshTimer,
        bindSearchViewportRuntime: bindSearchViewportRuntime,
        beginSearchLoadingFeedback: beginSearchLoadingFeedback,
        bindPowerUserShortcuts: bindPowerUserShortcuts,
        captureSearchScrollPosition: captureSearchScrollPosition,
        clearSelection: clearSelection,
        clearAnalyticsRefreshTimer: clearAnalyticsRefreshTimer,
        closeWorkflowAnalytics: closeWorkflowAnalytics,
        ensureEffectDialog: ensureEffectDialog,
        focusSearchResults: focusSearchResultsTable,
        focusSearchToolbar: focusSearchToolbar,
        onBeforeSmartTableRebind: onBeforeSmartTableRebind,
        onSearchMatched: onSearchMatched,
        onSmartTableInitialise: onSmartTableInitialise,
        openWorkflowAnalytics: openWorkflowAnalytics,
        runExport: runExport,
        scrollToSearchResultsToolbar: scrollToSearchResultsToolbar,
        scrollToSearchFilters: scrollToSearchFilters,
        selectVisibleRows: selectVisibleRows,
        setSearchActionBusy: setSearchActionBusy,
        shouldAllowDialogEffect: shouldAllowDialogEffect,
        syncSmartControlAvailability: syncSmartControlAvailability,
        unbindSearchViewportRuntime: unbindSearchViewportRuntime,
        unbindPowerUserShortcuts: unbindPowerUserShortcuts
    };
});

