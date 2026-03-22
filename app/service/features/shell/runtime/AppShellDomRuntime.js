sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntimeStringConstants"
], function (ThemeDomRuntime, JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;

    function resolveShellHeaderHostDom(oController) {
        var oShellHeaderControl = oController && oController.byId && oController.byId("appShellHeaderHost");
        return oShellHeaderControl && typeof oShellHeaderControl[METHODS.GET_DOM_REF] === TYPE_FUNCTION && oShellHeaderControl[METHODS.GET_DOM_REF]();
    }

    function syncShellFlexItem(oController) {
        var oLayout = oController && oController.byId && oController.byId("mainFcl");
        var oLayoutData;

        if (!oLayout || typeof oLayout.setFitContainer !== TYPE_FUNCTION) {
            return false;
        }
        oLayout.setFitContainer(true);
        oLayoutData = oLayout && typeof oLayout.getLayoutData === TYPE_FUNCTION && oLayout.getLayoutData();
        if (oLayoutData && typeof oLayoutData.setGrowFactor === TYPE_FUNCTION) {
            oLayoutData.setGrowFactor(1);
            oLayoutData.setShrinkFactor(1);
            oLayoutData.setBaseSize("0%");
        }
        return true;
    }

    function syncShellMetricVars(oController, oRootNode) {
        var oShellHeader = resolveShellHeaderHostDom(oController);
        var oRect;
        var iBottom;
        if (!oRootNode || !oShellHeader || typeof oShellHeader.getBoundingClientRect !== TYPE_FUNCTION) {
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
        var oMainDom = oMainHost && typeof oMainHost[METHODS.GET_DOM_REF] === TYPE_FUNCTION && oMainHost[METHODS.GET_DOM_REF]();
        if (oEvent && typeof oEvent.preventDefault === TYPE_FUNCTION) {
            oEvent.preventDefault();
        }
        if (!oMainDom || typeof oMainDom[METHODS.FOCUS] !== TYPE_FUNCTION) {
            return;
        }
        if (!oMainDom.getAttribute("tabindex")) {
            oMainDom.setAttribute("tabindex", "-1");
        }
        oMainDom[METHODS.FOCUS]();
    }

    function notifyBackgroundResize(oBackgroundRuntime, sPhase) {
        if (!oBackgroundRuntime) {
            return;
        }
        if (sPhase === "start" && typeof oBackgroundRuntime.onResizeStart === TYPE_FUNCTION) {
            oBackgroundRuntime.onResizeStart();
            return;
        }
        if (sPhase === "end" && typeof oBackgroundRuntime.onResizeEnd === TYPE_FUNCTION) {
            oBackgroundRuntime.onResizeEnd();
        }
    }

    return {
        focusMainContent: focusMainContent,
        notifyBackgroundResize: notifyBackgroundResize,
        resolveShellHeaderHostDom: resolveShellHeaderHostDom,
        syncShellFlexItem: syncShellFlexItem,
        syncShellMetricVars: syncShellMetricVars
    };
});
