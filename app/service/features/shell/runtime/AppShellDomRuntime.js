sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeDomRuntime"
], function (ThemeDomRuntime) {
    "use strict";

    function getShellFlexItem(oController) {
        var oLayout = oController && oController.byId && oController.byId("mainFcl");
        var oDomRef = oLayout && oLayout.getDomRef && oLayout.getDomRef();
        var oFlexItem = oDomRef && oDomRef.parentElement;
        if (!oFlexItem || !oFlexItem.classList || !oFlexItem.classList.contains("sapMFlexItem")) {
            return null;
        }
        return oFlexItem;
    }

    function syncShellFlexItem(oController) {
        var oFlexItem = getShellFlexItem(oController);
        if (!oFlexItem) {
            return;
        }
        ThemeDomRuntime.setStyleProperties([oFlexItem], {
            "flex": "1 1 auto",
            "min-height": "0",
            "height": "auto"
        });
    }

    function syncShellMetricVars(oController, oRootNode) {
        var oShellHeaderControl = oController && oController.byId && oController.byId("appShellHeaderHost");
        var oShellHeader = oShellHeaderControl && oShellHeaderControl.getDomRef && oShellHeaderControl.getDomRef();
        var oRect;
        var iBottom;
        if (!oRootNode || !oShellHeader || !oShellHeader.getBoundingClientRect) {
            return;
        }
        oRect = oShellHeader.getBoundingClientRect();
        iBottom = Math.max(88, Math.round(oRect.bottom + 14));
        ThemeDomRuntime.setStyleProperties([oRootNode], {
            "--app-shell-height": Math.round(oRect.height) + "px",
            "--app-shell-offset": iBottom + "px"
        });
    }

    function focusMainContent(oController, oEvent) {
        var oMainHost = oController && oController.byId && oController.byId("mainContentHost");
        var oMainDom = oMainHost && oMainHost.getDomRef && oMainHost.getDomRef();
        if (oEvent && oEvent.preventDefault) {
            oEvent.preventDefault();
        }
        if (!oMainDom || typeof oMainDom.focus !== "function") {
            return;
        }
        if (!oMainDom.getAttribute("tabindex")) {
            oMainDom.setAttribute("tabindex", "-1");
        }
        oMainDom.focus();
    }

    function notifyBackgroundResize(oBackgroundRuntime, sPhase) {
        if (!oBackgroundRuntime) {
            return;
        }
        if (sPhase === "start" && typeof oBackgroundRuntime.onResizeStart === "function") {
            oBackgroundRuntime.onResizeStart();
            return;
        }
        if (sPhase === "end" && typeof oBackgroundRuntime.onResizeEnd === "function") {
            oBackgroundRuntime.onResizeEnd();
        }
    }

    return {
        focusMainContent: focusMainContent,
        notifyBackgroundResize: notifyBackgroundResize,
        syncShellFlexItem: syncShellFlexItem,
        syncShellMetricVars: syncShellMetricVars
    };
});
