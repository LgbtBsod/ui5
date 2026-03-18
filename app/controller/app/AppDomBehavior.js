sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ShellGlobalsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Ui5RuntimeFacade"
], function (ModelStateRuntime, ThemeDomRuntime, SchedulingRuntime, ModelContracts, ShellGlobalsRuntime, Ui5RuntimeFacade) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var APP_VIEW_MODEL = MODELS.APP_VIEW;
    var _RESIZE_END_DELAY_MS = 520;
    var _RESIZE_SAFETY_DELAY_MS = 1800;
    var _RESIZE_CLASS = "chkResizing";

    function getDomRuntimeState(oController) {
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

    function getBackgroundRuntime() {
        return ShellGlobalsRuntime.getBackgroundRuntime();
    }

    function getAppContainerDom(oController) {
        var oRoot = oController && oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
        if (oRoot && oRoot.closest) {
            return oRoot.closest(".chkAppRoot") || oRoot;
        }
        return oRoot || null;
    }

    function _scheduleInvalidate(oController, oLayout) {
        var oRuntimeState = getDomRuntimeState(oController);
        oRuntimeState.resizeRafId = SchedulingRuntime.requestFrameOnce(oRuntimeState.resizeRafId, function () {
            oRuntimeState.resizeRafId = 0;
            if (oLayout && typeof oLayout.invalidate === "function") {
                oLayout.invalidate();
            }
        });
    }

    function _beginResizing(oController) {
        var oRuntimeState = getDomRuntimeState(oController);
        var oDoc = document && document.documentElement;
        ThemeDomRuntime.addClass([oDoc], _RESIZE_CLASS);
        oRuntimeState.resizeEndTimer = SchedulingRuntime.clearTimer(oRuntimeState.resizeEndTimer);
        oRuntimeState.resizeSafetyTimer = SchedulingRuntime.restartTimer(oRuntimeState.resizeSafetyTimer, function () {
            _settleResizing(oController);
        }, _RESIZE_SAFETY_DELAY_MS);
    }

    function _settleResizing(oController) {
        var oRuntimeState = getDomRuntimeState(oController);
        var oDoc = document && document.documentElement;

        oRuntimeState.resizeEndTimer = SchedulingRuntime.clearTimer(oRuntimeState.resizeEndTimer);
        oRuntimeState.resizeSafetyTimer = SchedulingRuntime.clearTimer(oRuntimeState.resizeSafetyTimer);
        ThemeDomRuntime.removeClass([oDoc], _RESIZE_CLASS);
        var oBackgroundRuntime = getBackgroundRuntime();
        if (oBackgroundRuntime && typeof oBackgroundRuntime.onResizeEnd === "function") {
            oBackgroundRuntime.onResizeEnd();
        }
    }

    function _scheduleResizeEnd(oController) {
        var oRuntimeState = getDomRuntimeState(oController);
        oRuntimeState.resizeEndTimer = SchedulingRuntime.restartTimer(oRuntimeState.resizeEndTimer, function () {
            _settleResizing(oController);
        }, _RESIZE_END_DELAY_MS);
    }

    return {
        _syncShellFlexAllocation: function () {
            var oLayout = this.byId("mainFcl");
            var oDomRef = oLayout && oLayout.getDomRef && oLayout.getDomRef();
            var oFlexItem = oDomRef && oDomRef.parentElement;
            if (!oFlexItem || !oFlexItem.classList || !oFlexItem.classList.contains("sapMFlexItem")) {
                return;
            }
            oFlexItem.style.flex = "1 1 auto";
            oFlexItem.style.minHeight = "0";
            oFlexItem.style.height = "auto";
        },

        _syncLayoutViewportGeometry: function () {
            var oLayout = this.byId("mainFcl");

            _beginResizing(this);
            var oBackgroundRuntime = getBackgroundRuntime();
            if (oBackgroundRuntime && typeof oBackgroundRuntime.onResizeStart === "function") {
                oBackgroundRuntime.onResizeStart();
            }
            _scheduleInvalidate(this, oLayout);
            _scheduleResizeEnd(this);
        },

        _syncStaticAreaScope: function () {
            var oStaticArea = Ui5RuntimeFacade.getStaticAreaRef();
            if (!oStaticArea) {
                return;
            }
            ThemeDomRuntime.addClass([oStaticArea], "chkApp");
            ThemeDomRuntime.addClass([oStaticArea], "chkSkin");
        },

        _applyCompactDensityClass: function () {
            var bCompact = !!ModelStateRuntime.read(this, APP_VIEW_MODEL, MODEL_PATHS.APP_VIEW_COMPACT_DENSITY, false);
            var oRoot = document && document.documentElement;
            var oBody = document && document.body;
            var oContainer = getAppContainerDom(this);
            var oAppDom = this.getView && this.getView().getDomRef && this.getView().getDomRef();
            ThemeDomRuntime.toggleClass([oRoot, oBody, oContainer, oAppDom], "appDensityCompact", bCompact);
        },

        _applyInvertedBlockSchemeClass: function () {
            var oRoot = document && document.documentElement;
            var oBody = document && document.body;
            var oContainer = getAppContainerDom(this);
            var oAppDom = this.getView && this.getView().getDomRef && this.getView().getDomRef();
            var bEnabled = !!ModelStateRuntime.read(this, APP_VIEW_MODEL, MODEL_PATHS.APP_VIEW_INVERTED_BLOCK_SCHEME, false);
            ThemeDomRuntime.toggleClass([oRoot, oBody, oContainer, oAppDom], "appInvertedBlockScheme", bEnabled);
        },

        _syncShellMetrics: function () {
            var oRoot = this.getView && this.getView().getDomRef && this.getView().getDomRef();
            var oShellHeader = oRoot && oRoot.querySelector && oRoot.querySelector(".appShellHeader");
            var oDoc = document && document.documentElement;
            var oRect;
            var iBottom;
            if (!oDoc || !oShellHeader || !oShellHeader.getBoundingClientRect) {
                return;
            }
            oRect = oShellHeader.getBoundingClientRect();
            iBottom = Math.max(88, Math.round(oRect.bottom + 14));
            ThemeDomRuntime.setStyleProperties([oDoc], {
                "--app-shell-height": Math.round(oRect.height) + "px",
                "--app-shell-offset": iBottom + "px"
            });
        },

        _scheduleShellLayoutRefresh: function () {
            var oRuntimeState = getDomRuntimeState(this);
            var that = this;
            oRuntimeState.shellRefreshRafId = SchedulingRuntime.restartFrame(oRuntimeState.shellRefreshRafId, function () {
                oRuntimeState.shellRefreshRafId = 0;
                SchedulingRuntime.nextFrame(function () {
                    that._syncShellFlexAllocation();
                    that._syncShellMetrics();
                    that._syncLayoutViewportGeometry();
                });
            });
        },

        _teardownAppDomRuntime: function () {
            var oRuntimeState = getDomRuntimeState(this);
            oRuntimeState.resizeRafId = SchedulingRuntime.clearFrame(oRuntimeState.resizeRafId);
            oRuntimeState.resizeEndTimer = SchedulingRuntime.clearTimer(oRuntimeState.resizeEndTimer);
            oRuntimeState.resizeSafetyTimer = SchedulingRuntime.clearTimer(oRuntimeState.resizeSafetyTimer);
            oRuntimeState.shellRefreshRafId = SchedulingRuntime.clearFrame(oRuntimeState.shellRefreshRafId);
            _settleResizing(this);
            this._oAppDomRuntimeState = null;
        }
    };
});
