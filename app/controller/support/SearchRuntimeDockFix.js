sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/util/ThemeDomRuntime"
], function (ControllerViewStateRuntime, SchedulingRuntime, ThemeDomRuntime) {
    "use strict";

    function resolveViewDom(oController) {
        return oController && oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
    }

    function resolveControlDom(oController, sId, sFallbackSelector) {
        var oControl = oController && oController.byId && oController.byId(sId);
        var oDomRef = oControl && oControl.getDomRef && oControl.getDomRef();
        if (!oDomRef && typeof document !== "undefined" && sFallbackSelector) {
            oDomRef = document.querySelector(sFallbackSelector);
        }
        return oDomRef || null;
    }

    function resolveResultsToolbarDom(oController) {
        var oViewDom = resolveViewDom(oController);
        return oViewDom && oViewDom.querySelector ? oViewDom.querySelector(".searchResultsTable .sapUiCompSmartTableToolbar") : null;
    }

    function resolveScrollHost(oController) {
        var oViewDom = resolveViewDom(oController);
        var oNode = oViewDom && oViewDom.parentElement;
        while (oNode && oNode !== document.body) {
            if (oNode.scrollHeight > oNode.clientHeight + 4) {
                return oNode;
            }
            oNode = oNode.parentElement;
        }
        return document.scrollingElement || document.documentElement || document.body;
    }

    function resolveHeight(oNode) {
        return oNode && oNode.getBoundingClientRect ? Math.max(0, Math.ceil(oNode.getBoundingClientRect().height || 0)) : 0;
    }

    function resolveOffsetWithinHost(oNode, oHost) {
        var oNodeRect;
        var oHostRect;
        if (!oNode || !oHost || !oNode.getBoundingClientRect || !oHost.getBoundingClientRect) {
            return 0;
        }
        oNodeRect = oNode.getBoundingClientRect();
        oHostRect = oHost.getBoundingClientRect();
        return Math.max(0, Math.round((oNodeRect.top - oHostRect.top) + (oHost.scrollTop || 0)));
    }

    function syncResultsToolbarFallback(oController, oResultsToolbar, iTableToolbarTop, iResultsToolbarHeight) {
        var oScrollHost = resolveScrollHost(oController);
        var oResultsShell = resolveControlDom(oController, "searchResultsShell", ".searchResultsTable");
        var iCurrentTranslate = Number((oResultsToolbar && oResultsToolbar.dataset && oResultsToolbar.dataset.searchRuntimeTranslateY) || 0);
        var iNaturalTop;
        var iShellTop;
        var iShellHeight;
        var iDesiredTranslate;
        var iMaxTranslate;
        var iTranslate;

        if (!oResultsToolbar || !oScrollHost || !oResultsShell) {
            return;
        }

        iNaturalTop = resolveOffsetWithinHost(oResultsToolbar, oScrollHost) - iCurrentTranslate;
        iShellTop = resolveOffsetWithinHost(oResultsShell, oScrollHost);
        iShellHeight = resolveHeight(oResultsShell);
        iDesiredTranslate = Math.max(0, Math.round((oScrollHost.scrollTop || 0) + iTableToolbarTop - iNaturalTop));
        iMaxTranslate = Math.max(0, iShellTop + iShellHeight - iResultsToolbarHeight - iNaturalTop - 8);
        iTranslate = Math.min(iDesiredTranslate, iMaxTranslate);

        oResultsToolbar.style.transform = iTranslate > 0 ? "translateY(" + iTranslate + "px)" : "";
        if (oResultsToolbar.dataset) {
            oResultsToolbar.dataset.searchRuntimeTranslateY = String(iTranslate);
        }
    }

    function resolveShellOffset(oController) {
        var oShellHeader = typeof document !== "undefined" ? document.querySelector(".appShellHeader") : null;
        var oScrollHost = resolveScrollHost(oController);
        var iShellBottom = oShellHeader && oShellHeader.getBoundingClientRect ? Math.ceil(oShellHeader.getBoundingClientRect().bottom || 0) : 0;
        var iHostTop = oScrollHost && oScrollHost.getBoundingClientRect ? Math.ceil(oScrollHost.getBoundingClientRect().top || 0) : 0;
        return Math.max(8, iShellBottom - iHostTop + 2);
    }

    function resolveCounts(oInnerTable) {
        var aItems = oInnerTable && oInnerTable.getItems ? (oInnerTable.getItems() || []) : [];
        var oBinding = oInnerTable && oInnerTable.getBinding ? oInnerTable.getBinding("items") : null;
        var iVisible = aItems.length;
        var iTotal = oBinding && oBinding.getLength ? Number(oBinding.getLength()) : iVisible;
        if (!Number.isFinite(iTotal) || iTotal < 0) {
            iTotal = iVisible;
        }
        return {
            visible: iVisible,
            total: iTotal,
            hasRows: iTotal > 0 || iVisible > 0
        };
    }

    function enforceSearchTable(oController) {
        var oSmartTable = oController && oController.byId && oController.byId("searchSmartTable");
        var oInnerTable = oSmartTable && oSmartTable.getTable && oSmartTable.getTable();
        var mCounts;
        if (!oInnerTable) {
            return;
        }
        if (oInnerTable.setMode) {
            oInnerTable.setMode("MultiSelect");
        }
        if (oInnerTable.setIncludeItemInSelection) {
            oInnerTable.setIncludeItemInSelection(true);
        }
        mCounts = resolveCounts(oInnerTable);
        ControllerViewStateRuntime.setMany(oController, {
            "/hasRows": mCounts.hasRows,
            "/canExport": mCounts.hasRows
        });
    }

    function syncStickyLayout(oController) {
        var oViewDom = resolveViewDom(oController);
        var oFilterCard = resolveControlDom(oController, "searchFilterCard", ".searchFilterCardDense");
        var oActionRail = resolveControlDom(oController, "searchResultsActionRail", ".searchResultsActionRail");
        var oToolbarRail = resolveControlDom(oController, "smartTableCustomToolbar", ".searchSmartToolbarRail");
        var oResultsToolbar = resolveResultsToolbarDom(oController);
        var iBaseTop;
        var iFilterHeight;
        var iActionHeight;
        var iToolbarHeight;
        var iResultsToolbarHeight;
        var iActionTop;
        var iToolbarTop;
        var iTableToolbarTop;

        if (!oViewDom || !oFilterCard || !oActionRail || !oToolbarRail) {
            return;
        }

        iBaseTop = resolveShellOffset(oController);
        iFilterHeight = resolveHeight(oFilterCard);
        iActionHeight = resolveHeight(oActionRail);
        iToolbarHeight = resolveHeight(oToolbarRail);
        iResultsToolbarHeight = resolveHeight(oResultsToolbar);
        iActionTop = iBaseTop + iFilterHeight + (iFilterHeight && iActionHeight ? 6 : 0);
        iToolbarTop = iActionTop + iActionHeight + (iActionHeight && iToolbarHeight ? 6 : 0);
        iTableToolbarTop = iToolbarTop + iToolbarHeight + (iToolbarHeight && iResultsToolbarHeight ? 8 : 0);

        ThemeDomRuntime.setStyleProperties([oViewDom], {
            "--search-sticky-filter-top": iBaseTop + "px",
            "--search-sticky-action-top": iActionTop + "px",
            "--search-sticky-toolbar-top": iToolbarTop + "px",
            "--search-sticky-table-toolbar-top": iTableToolbarTop + "px"
        });
        ThemeDomRuntime.setStyleProperty([oFilterCard], "top", iBaseTop + "px");
        ThemeDomRuntime.setStyleProperty([oActionRail], "top", iActionTop + "px");
        ThemeDomRuntime.setStyleProperty([oToolbarRail], "top", iToolbarTop + "px");
        ThemeDomRuntime.setStyleProperty([oResultsToolbar], "top", iTableToolbarTop + "px");
        syncResultsToolbarFallback(oController, oResultsToolbar, iTableToolbarTop, iResultsToolbarHeight);
    }

    function clearSyncTimers(oController) {
        oController._iSearchRuntimeDockFixTimer = SchedulingRuntime.clearTimer(oController._iSearchRuntimeDockFixTimer);
        oController._iSearchRuntimeDockFixLateTimer = SchedulingRuntime.clearTimer(oController._iSearchRuntimeDockFixLateTimer);
    }

    function runSync(oController) {
        enforceSearchTable(oController);
        syncStickyLayout(oController);
    }

    function scheduleSync(oController, bImmediate) {
        clearSyncTimers(oController);
        if (bImmediate) {
            SchedulingRuntime.nextDoubleFrame(function () {
                runSync(oController);
            });
        } else {
            oController._iSearchRuntimeDockFixTimer = SchedulingRuntime.restartTimer(oController._iSearchRuntimeDockFixTimer, function () {
                runSync(oController);
            }, 96);
        }
        oController._iSearchRuntimeDockFixLateTimer = SchedulingRuntime.restartTimer(oController._iSearchRuntimeDockFixLateTimer, function () {
            runSync(oController);
        }, 320);
    }

    function bindTableUpdate(oController) {
        var oSmartTable = oController && oController.byId && oController.byId("searchSmartTable");
        var oInnerTable = oSmartTable && oSmartTable.getTable && oSmartTable.getTable();
        if (!oInnerTable || oInnerTable.data("searchRuntimeDockFixBound")) {
            return;
        }
        if (oInnerTable.attachUpdateFinished) {
            oInnerTable.attachUpdateFinished(function () {
                scheduleSync(oController, true);
            });
        }
        oInnerTable.data("searchRuntimeDockFixBound", true);
    }

    function bindResizeObserver(oController) {
        var aTargets;
        if (typeof window === "undefined" || typeof window.ResizeObserver !== "function") {
            return;
        }
        if (!oController._oSearchRuntimeDockFixObserver) {
            oController._oSearchRuntimeDockFixObserver = new window.ResizeObserver(function () {
                scheduleSync(oController, false);
            });
        }
        aTargets = [
            resolveViewDom(oController),
            resolveControlDom(oController, "searchFilterCard", ".searchFilterCardDense"),
            resolveControlDom(oController, "searchResultsActionRail", ".searchResultsActionRail"),
            resolveControlDom(oController, "smartTableCustomToolbar", ".searchSmartToolbarRail"),
            resolveResultsToolbarDom(oController)
        ].filter(Boolean);
        aTargets.forEach(function (oTarget) {
            oController._oSearchRuntimeDockFixObserver.observe(oTarget);
        });
    }

    function bindScrollHost(oController) {
        var oScrollHost = resolveScrollHost(oController);
        if (!oScrollHost || oController._oSearchRuntimeDockFixScrollHost === oScrollHost) {
            return;
        }
        if (oController._oSearchRuntimeDockFixScrollHost && oController._fnSearchRuntimeDockFixScroll) {
            oController._oSearchRuntimeDockFixScrollHost.removeEventListener("scroll", oController._fnSearchRuntimeDockFixScroll);
        }
        oController._fnSearchRuntimeDockFixScroll = oController._fnSearchRuntimeDockFixScroll || function () {
            if (oController._iSearchRuntimeDockFixFrame) {
                return;
            }
            oController._iSearchRuntimeDockFixFrame = window.requestAnimationFrame(function () {
                oController._iSearchRuntimeDockFixFrame = 0;
                runSync(oController);
            });
        };
        oScrollHost.addEventListener("scroll", oController._fnSearchRuntimeDockFixScroll, { passive: true });
        oController._oSearchRuntimeDockFixScrollHost = oScrollHost;
    }

    function bind(oController) {
        if (!oController) {
            return;
        }
        bindTableUpdate(oController);
        bindResizeObserver(oController);
        bindScrollHost(oController);
        scheduleSync(oController, true);
    }

    return {
        bind: bind,
        sync: scheduleSync
    };
});
