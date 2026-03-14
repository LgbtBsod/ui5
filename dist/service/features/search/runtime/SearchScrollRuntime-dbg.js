sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchViewportContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/SearchUiContracts"
], function (ControllerModelRuntime, ControllerViewStateRuntime, ModelStateRuntime, SchedulingRuntime, SearchViewportContracts, SearchUiContracts) {
    "use strict";

    var SEARCH_ANCHOR_SCROLL_MARGIN_PX = SearchUiContracts.VIEWPORT.ANCHOR_SCROLL_MARGIN_PX;
    var SEARCH_SCROLL_NAV_TOP_PX = SearchUiContracts.VIEWPORT.SCROLL_NAV_TOP_PX;
    var SEARCH_RESULTS_NAV_EXTRA_PX = SearchUiContracts.VIEWPORT.RESULTS_NAV_EXTRA_PX;
    var SEARCH_POST_ANCHOR_SYNC_DELAY_MS = SearchUiContracts.VIEWPORT.POST_ANCHOR_SYNC_DELAY_MS;
    var STATE_MODEL = SearchViewportContracts.MODELS.STATE;
    var STATE_PATHS = SearchViewportContracts.STATE_PATHS;
    var VIEW_PATHS = SearchViewportContracts.VIEW_PATHS;

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

    function schedulePostAnchorSync(oController, mHooks) {
        oController._iSearchAnchorSyncTimer = SchedulingRuntime.restartTimer(
            oController._iSearchAnchorSyncTimer,
            function () {
                mHooks.syncViewportLayout();
                syncSearchScrollAffordances(oController, mHooks.resolveToolbarDom());
            },
            SEARCH_POST_ANCHOR_SYNC_DELAY_MS
        );
    }

    function syncSearchScrollAffordances(oController, oToolbarDom) {
        var oScrollHost = resolveSearchScrollHost(oController);
        var oResultsShell = oController.byId && oController.byId("searchResultsShell");
        var oResultsShellDom = oResultsShell && oResultsShell.getDomRef && oResultsShell.getDomRef();
        var iTop = oScrollHost ? Number(oScrollHost.scrollTop || 0) : 0;
        var oHostRect;
        var oAnchorRect;
        var iAnchorTop = 0;
        var oAnchorDom = oResultsShellDom || oToolbarDom;
        ControllerViewStateRuntime.set(oController, VIEW_PATHS.SCROLL_NAV_VISIBLE, iTop > SEARCH_SCROLL_NAV_TOP_PX);
        if (oScrollHost && oAnchorDom && oScrollHost.getBoundingClientRect && oAnchorDom.getBoundingClientRect) {
            oHostRect = oScrollHost.getBoundingClientRect();
            oAnchorRect = oAnchorDom.getBoundingClientRect();
            iAnchorTop = iTop + (oAnchorRect.top - oHostRect.top);
        }
        ControllerViewStateRuntime.set(oController, VIEW_PATHS.RESULTS_TOOLBAR_NAV_VISIBLE, !!oAnchorDom && iTop > (iAnchorTop + SEARCH_RESULTS_NAV_EXTRA_PX));
    }

    function captureSearchScrollPosition(oController) {
        var oScrollHost = resolveSearchScrollHost(oController);
        if (!ControllerModelRuntime.state(oController)) {
            return;
        }
        ModelStateRuntime.write(oController, STATE_MODEL, STATE_PATHS.SEARCH_SCROLL_STATE, {
            hostTop: oScrollHost ? oScrollHost.scrollTop : 0
        });
    }

    function restoreSearchScrollPosition(oController, mHooks) {
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
            mHooks.syncViewportLayout();
            syncSearchScrollAffordances(oController, mHooks.resolveToolbarDom());
        });
    }

    function scrollToTarget(oController, oTargetDom, iStickyOffset, mHooks) {
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
        syncSearchScrollAffordances(oController, mHooks.resolveToolbarDom());
        schedulePostAnchorSync(oController, mHooks);
        return Promise.resolve(true);
    }

    return {
        resolveSearchScrollHost: resolveSearchScrollHost,
        syncSearchScrollAffordances: syncSearchScrollAffordances,
        captureSearchScrollPosition: captureSearchScrollPosition,
        restoreSearchScrollPosition: restoreSearchScrollPosition,
        scrollToTarget: scrollToTarget
    };
});
