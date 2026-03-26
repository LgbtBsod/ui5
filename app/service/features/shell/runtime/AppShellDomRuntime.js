sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime"
], function (ThemeDomRuntime, SchedulingRuntime, JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;
    var RESIZE_CLASS = "chkResizing";

    function ensureDomRuntimeState(oController) {
        if (!oController) {
            return {
                resizeRafId: 0,
                resizeEndTimer: 0,
                resizeSafetyTimer: 0,
                shellRefreshRafId: 0
            };
        }
        oController._oAppDomRuntimeState = oController._oAppDomRuntimeState || {
            resizeRafId: 0,
            resizeEndTimer: 0,
            resizeSafetyTimer: 0,
            shellRefreshRafId: 0
        };
        return oController._oAppDomRuntimeState;
    }

    function resolveViewDom(oController) {
        var oView = oController && oController.getView && oController.getView();
        return oView && typeof oView[METHODS.GET_DOM_REF] === TYPE_FUNCTION && oView[METHODS.GET_DOM_REF]();
    }

    function resolveAppContainerDom(oController) {
        var oRoot = resolveViewDom(oController);
        if (oRoot && typeof oRoot.closest === TYPE_FUNCTION) {
            return oRoot.closest(".chkAppRoot") || oRoot;
        }
        return oRoot || null;
    }

    function getAppDomTargets(oController) {
        var oNodes = ThemeDomRuntime.getNodes();
        return [oNodes.root || null, oNodes.body || null, resolveAppContainerDom(oController), resolveViewDom(oController)];
    }

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

    function scheduleInvalidate(oController, oLayout) {
        var oRuntimeState = ensureDomRuntimeState(oController);
        oRuntimeState.resizeRafId = SchedulingRuntime.requestFrameOnce(oRuntimeState.resizeRafId, function () {
            oRuntimeState.resizeRafId = 0;
            if (oLayout && typeof oLayout.invalidate === TYPE_FUNCTION) {
                oLayout.invalidate();
            }
        });
    }

    function settleResizeCycle(oController, iResizeEndDelayMs, iResizeSafetyDelayMs) {
        var oRuntimeState = ensureDomRuntimeState(oController);
        var oRoot = ThemeDomRuntime.getNodes().root;
        oRuntimeState.resizeEndTimer = SchedulingRuntime.clearTimer(oRuntimeState.resizeEndTimer);
        oRuntimeState.resizeSafetyTimer = SchedulingRuntime.clearTimer(oRuntimeState.resizeSafetyTimer);
        ThemeDomRuntime.removeClass([oRoot], RESIZE_CLASS);
        return {
            resizeEndDelayMs: iResizeEndDelayMs,
            resizeSafetyDelayMs: iResizeSafetyDelayMs
        };
    }

    function beginResizeCycle(oController, iResizeEndDelayMs, iResizeSafetyDelayMs, fnOnSettle) {
        var oRuntimeState = ensureDomRuntimeState(oController);
        var oRoot = ThemeDomRuntime.getNodes().root;
        ThemeDomRuntime.addClass([oRoot], RESIZE_CLASS);
        oRuntimeState.resizeEndTimer = SchedulingRuntime.clearTimer(oRuntimeState.resizeEndTimer);
        oRuntimeState.resizeSafetyTimer = SchedulingRuntime.restartTimer(oRuntimeState.resizeSafetyTimer, function () {
            settleResizeCycle(oController, iResizeEndDelayMs, iResizeSafetyDelayMs);
            if (typeof fnOnSettle === TYPE_FUNCTION) {
                fnOnSettle();
            }
        }, iResizeSafetyDelayMs);
    }

    function scheduleResizeEnd(oController, iResizeEndDelayMs, iResizeSafetyDelayMs, fnOnSettle) {
        var oRuntimeState = ensureDomRuntimeState(oController);
        oRuntimeState.resizeEndTimer = SchedulingRuntime.restartTimer(oRuntimeState.resizeEndTimer, function () {
            settleResizeCycle(oController, iResizeEndDelayMs, iResizeSafetyDelayMs);
            if (typeof fnOnSettle === TYPE_FUNCTION) {
                fnOnSettle();
            }
        }, iResizeEndDelayMs);
    }

    function teardownResizeCycle(oController) {
        var oRuntimeState = ensureDomRuntimeState(oController);
        oRuntimeState.resizeRafId = SchedulingRuntime.clearFrame(oRuntimeState.resizeRafId);
        oRuntimeState.resizeEndTimer = SchedulingRuntime.clearTimer(oRuntimeState.resizeEndTimer);
        oRuntimeState.resizeSafetyTimer = SchedulingRuntime.clearTimer(oRuntimeState.resizeSafetyTimer);
        oRuntimeState.shellRefreshRafId = SchedulingRuntime.clearFrame(oRuntimeState.shellRefreshRafId);
        ThemeDomRuntime.removeClass([ThemeDomRuntime.getNodes().root], RESIZE_CLASS);
        if (oController) {
            oController._oAppDomRuntimeState = null;
        }
    }

    return {
        beginResizeCycle: beginResizeCycle,
        focusMainContent: focusMainContent,
        getAppDomTargets: getAppDomTargets,
        notifyBackgroundResize: notifyBackgroundResize,
        resolveViewDom: resolveViewDom,
        resolveShellHeaderHostDom: resolveShellHeaderHostDom,
        scheduleInvalidate: scheduleInvalidate,
        scheduleResizeEnd: scheduleResizeEnd,
        syncShellFlexItem: syncShellFlexItem,
        syncShellMetricVars: syncShellMetricVars,
        teardownResizeCycle: teardownResizeCycle,
        ensureDomRuntimeState: ensureDomRuntimeState
    };
});
