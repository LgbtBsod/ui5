sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailActionConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime"
], function (DetailActionConstants, ControllerViewStateRuntime) {
    "use strict";

    function remToPx(fRem) {
        var iRootSize = parseFloat(window.getComputedStyle(document.documentElement).fontSize || "16");
        return Math.round(Number(fRem || 0) * (Number.isFinite(iRootSize) && iRootSize > 0 ? iRootSize : 16));
    }

    function syncAdaptiveDetailViewport(oController) {
        var oView = oController.getView && oController.getView();
        var oObjectPage = oController.byId("detailObjectPage");
        var oDom = (oObjectPage && oObjectPage.getDomRef && oObjectPage.getDomRef()) || (oView && oView.getDomRef && oView.getDomRef());
        var iWidth;
        var bNarrow;
        if (!oDom) {
            return;
        }
        iWidth = Math.round((oDom.getBoundingClientRect && oDom.getBoundingClientRect().width) || 0);
        bNarrow = iWidth > 0 && iWidth <= remToPx(DetailActionConstants.DETAIL_NARROW_VIEWPORT_REM);
        ControllerViewStateRuntime.setFlag(oController, "/narrowDetailViewport", bNarrow);
        if (oView && typeof oView.toggleStyleClass === "function") {
            oView.toggleStyleClass("detailViewportNarrow", bNarrow);
        }
        oController._syncViewportPinnedControlRail();
    }

    function bindAdaptiveDetailViewport(oController) {
        var oView = oController.getView && oController.getView();
        var oDom = oView && oView.getDomRef && oView.getDomRef();
        if (!oDom) {
            return;
        }
        if (!oController._fnAdaptiveViewportSync) {
            oController._fnAdaptiveViewportSync = syncAdaptiveDetailViewport.bind(null, oController);
        }
        unbindAdaptiveDetailViewport(oController);
        if (typeof ResizeObserver === "function") {
            oController._oAdaptiveViewportResizeObserver = new ResizeObserver(oController._fnAdaptiveViewportSync);
            oController._oAdaptiveViewportResizeObserver.observe(oDom);
        }
        window.addEventListener("resize", oController._fnAdaptiveViewportSync, true);
        syncAdaptiveDetailViewport(oController);
    }

    function unbindAdaptiveDetailViewport(oController) {
        if (oController._oAdaptiveViewportResizeObserver && typeof oController._oAdaptiveViewportResizeObserver.disconnect === "function") {
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
