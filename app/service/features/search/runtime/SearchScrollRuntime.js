sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchViewportContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchContracts"
], function (ControllerModelRuntime, ModelStateRuntime, SchedulingRuntime, SearchViewportContracts, SearchUiContracts) {
    "use strict";

    var SEARCH_ANCHOR_SCROLL_MARGIN_PX = SearchUiContracts.VIEWPORT.ANCHOR_SCROLL_MARGIN_PX;
    var STATE_MODEL = SearchViewportContracts.MODELS.STATE;
    var STATE_PATHS = SearchViewportContracts.STATE_PATHS;

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
            if (mHooks && mHooks.syncViewportLayout) {
                mHooks.syncViewportLayout();
            }
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
        if (mHooks && mHooks.syncViewportLayout) {
            mHooks.syncViewportLayout();
        }
        return Promise.resolve(true);
    }

    return {
        resolveSearchScrollHost: resolveSearchScrollHost,
        captureSearchScrollPosition: captureSearchScrollPosition,
        restoreSearchScrollPosition: restoreSearchScrollPosition,
        scrollToTarget: scrollToTarget
    };
});
