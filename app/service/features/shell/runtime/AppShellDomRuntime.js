sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeDomRuntime"
], function (ThemeDomRuntime) {
    "use strict";

    function syncShellFlexItem(oController) {
        var oLayout = oController && oController.byId && oController.byId("mainFcl");
        var oLayoutData;

        if (!oLayout || typeof oLayout.setFitContainer !== "function") {
            return false;
        }
        oLayout.setFitContainer(true);
        oLayoutData = oLayout.getLayoutData && oLayout.getLayoutData();
        if (oLayoutData && typeof oLayoutData.setGrowFactor === "function") {
            oLayoutData.setGrowFactor(1);
            oLayoutData.setShrinkFactor(1);
            oLayoutData.setBaseSize("0%");
        }
        return true;
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
