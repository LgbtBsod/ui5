sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchScrollRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchStickyLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/SearchUiContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EventDelegateRuntime"
], function (SearchScrollRuntime, SearchStickyLayoutRuntime, SchedulingRuntime, SearchUiContracts, JsRuntime, EventDelegateRuntime) {
    "use strict";

    var SEARCH_VIEWPORT_LAYOUT_DEBOUNCE_MS = SearchUiContracts.VIEWPORT.LAYOUT_DEBOUNCE_MS;

    function clearSearchViewportSyncTimer(oController) {
        oController._iSearchViewportSyncTimer = SchedulingRuntime.clearTimer(oController._iSearchViewportSyncTimer);
    }

    function syncSearchViewportLayout(oController, bForce) {
        var oScrollHost = SearchScrollRuntime.resolveSearchScrollHost(oController);
        var sLayoutKey = SearchStickyLayoutRuntime.buildSearchViewportLayoutKey(oController, oScrollHost);
        if (!bForce && sLayoutKey && oController._sSearchTableLayoutKey === sLayoutKey) {
            return false;
        }
        SearchStickyLayoutRuntime.syncSearchViewportLayout(oController, oScrollHost);
        oController._sSearchTableLayoutKey = sLayoutKey;
        return true;
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
        if (typeof window === "undefined" || typeof window.ResizeObserver !== JsRuntime.TYPEOF.FUNCTION) {
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

    return {
        bindSearchViewportRuntime: bindSearchViewportRuntime,
        scheduleSearchViewportSync: scheduleSearchViewportSync,
        syncSearchViewportLayout: syncSearchViewportLayout,
        unbindSearchViewportRuntime: unbindSearchViewportRuntime
    };
});
