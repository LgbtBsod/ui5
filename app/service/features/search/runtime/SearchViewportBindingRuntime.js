sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchScrollRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchStickyLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/SearchUiContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/JsRuntimeContracts"
], function (SearchScrollRuntime, SearchStickyLayoutRuntime, SchedulingRuntime, SearchUiContracts, JsRuntimeContracts) {
    "use strict";

    var SEARCH_VIEWPORT_LAYOUT_DEBOUNCE_MS = SearchUiContracts.VIEWPORT.LAYOUT_DEBOUNCE_MS;

    function clearSearchViewportSyncTimer(oController) {
        oController._iSearchViewportSyncTimer = SchedulingRuntime.clearTimer(oController._iSearchViewportSyncTimer);
    }

    function syncSearchViewportLayout(oController) {
        SearchStickyLayoutRuntime.syncSearchViewportLayout(
            oController,
            SearchScrollRuntime.resolveSearchScrollHost(oController)
        );
    }

    function syncSearchScrollAffordances(oController) {
        SearchScrollRuntime.syncSearchScrollAffordances(
            oController,
            SearchStickyLayoutRuntime.resolveSearchTableToolbarDom(oController)
        );
    }

    function flushSearchViewportSync(oController) {
        oController._iSearchViewportSyncRaf = SchedulingRuntime.requestFrameOnce(oController._iSearchViewportSyncRaf, function () {
            oController._iSearchViewportSyncRaf = 0;
            syncSearchViewportLayout(oController);
            syncSearchScrollAffordances(oController);
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
        if (typeof window === "undefined" || typeof window.ResizeObserver !== JsRuntimeContracts.TYPE_FUNCTION) {
            return;
        }
        if (!oController._oSearchViewportResizeObserver) {
            oController._oSearchViewportResizeObserver = new window.ResizeObserver(function () {
                scheduleSearchViewportSync(oController, false);
            });
        }
        aPrevTargets = oController._aSearchViewportObserverTargets || [];
        aNextTargets = SearchStickyLayoutRuntime.resolveSearchViewportObserverTargets(oController, oScrollHost);
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
        var oScrollHost = SearchScrollRuntime.resolveSearchScrollHost(oController);
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
        oController._iSearchAnchorSyncTimer = SchedulingRuntime.clearTimer(oController._iSearchAnchorSyncTimer);
        oController._iSearchViewportSyncRaf = SchedulingRuntime.clearFrame(oController._iSearchViewportSyncRaf);
        oController._oSearchViewportDelegate = null;
        oController._oSearchScrollHost = null;
        oController._fnSearchScrollSync = null;
        oController._fnSearchViewportResize = null;
        oController._sSearchTableLayoutKey = "";
        oController._iSearchViewportSyncTimer = 0;
        oController._iSearchAnchorSyncTimer = 0;
        oController._iSearchViewportSyncRaf = 0;
    }

    return {
        bindSearchViewportRuntime: bindSearchViewportRuntime,
        scheduleSearchViewportSync: scheduleSearchViewportSync,
        syncSearchScrollAffordances: syncSearchScrollAffordances,
        syncSearchViewportLayout: syncSearchViewportLayout,
        unbindSearchViewportRuntime: unbindSearchViewportRuntime
    };
});
