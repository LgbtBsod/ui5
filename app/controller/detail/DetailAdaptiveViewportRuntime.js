sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailActionConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailAttachmentViewState",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntimeStringConstants"
], function (DetailActionConstants, ControllerViewStateRuntime, DetailAttachmentViewState, JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;

    function remToPx(fRem) {
        var iRootSize = parseFloat(window.getComputedStyle(document.documentElement).fontSize || "16");
        return Math.round(Number(fRem || 0) * (Number.isFinite(iRootSize) && iRootSize > 0 ? iRootSize : 16));
    }

    function syncAdaptiveDetailViewport(oController) {
        var oView = oController && typeof oController.getView === TYPE_FUNCTION && oController.getView();
        var oObjectPage = oController.byId("detailObjectPage");
        var oDom = (oObjectPage && typeof oObjectPage[METHODS.GET_DOM_REF] === TYPE_FUNCTION && oObjectPage[METHODS.GET_DOM_REF]())
            || (oView && typeof oView[METHODS.GET_DOM_REF] === TYPE_FUNCTION && oView[METHODS.GET_DOM_REF]());
        var iWidth;
        var bNarrow;
        if (!oDom) {
            return;
        }
        iWidth = Math.round((oDom.getBoundingClientRect && oDom.getBoundingClientRect().width) || 0);
        bNarrow = iWidth > 0 && iWidth <= remToPx(DetailActionConstants.DETAIL_NARROW_VIEWPORT_REM);
        ControllerViewStateRuntime.setFlag(oController, "/narrowDetailViewport", bNarrow);
        DetailAttachmentViewState.sync(oController);
        if (oView && typeof oView.toggleStyleClass === TYPE_FUNCTION) {
            oView.toggleStyleClass("detailViewportNarrow", bNarrow);
        }
        if (oController && typeof oController._syncViewportPinnedControlRail === TYPE_FUNCTION) {
            oController._syncViewportPinnedControlRail();
        }
    }

    function bindAdaptiveDetailViewport(oController) {
        var oView = oController && typeof oController.getView === TYPE_FUNCTION && oController.getView();
        var oDom = oView && typeof oView[METHODS.GET_DOM_REF] === TYPE_FUNCTION && oView[METHODS.GET_DOM_REF]();
        if (!oDom) {
            return;
        }
        if (!oController._fnAdaptiveViewportSync) {
            oController._fnAdaptiveViewportSync = syncAdaptiveDetailViewport.bind(null, oController);
        }
        unbindAdaptiveDetailViewport(oController);
        if (typeof ResizeObserver === TYPE_FUNCTION) {
            oController._oAdaptiveViewportResizeObserver = new ResizeObserver(oController._fnAdaptiveViewportSync);
            oController._oAdaptiveViewportResizeObserver.observe(oDom);
        }
        window.addEventListener("resize", oController._fnAdaptiveViewportSync, true);
        syncAdaptiveDetailViewport(oController);
    }

    function unbindAdaptiveDetailViewport(oController) {
        if (oController._oAdaptiveViewportResizeObserver && typeof oController._oAdaptiveViewportResizeObserver.disconnect === TYPE_FUNCTION) {
            oController._oAdaptiveViewportResizeObserver.disconnect();
        }
        if (oController._fnAdaptiveViewportSync) {
            window.removeEventListener("resize", oController._fnAdaptiveViewportSync, true);
        }
        oController._oAdaptiveViewportResizeObserver = null;
    }

    return {
        bindAdaptiveDetailViewport: bindAdaptiveDetailViewport,
        syncAdaptiveDetailViewport: syncAdaptiveDetailViewport,
        unbindAdaptiveDetailViewport: unbindAdaptiveDetailViewport
    };
});
