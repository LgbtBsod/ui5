sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ShellGlobalsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Ui5RuntimeFacade"
], function (ModelStateRuntime, ThemeDomRuntime, SchedulingRuntime, ModelContracts, ShellGlobalsRuntime, Ui5RuntimeFacade) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var APP_VIEW_MODEL = MODELS.APP_VIEW;
    var _iResizeRafId = 0;
    var _iResizeEndTimer = 0;
    var _iResizeSafetyTimer = 0;
    var _iShellRefreshRafId = 0;
    var _RESIZE_END_DELAY_MS = 520;
    var _RESIZE_SAFETY_DELAY_MS = 1800;
    var _RESIZE_CLASS = "chkResizing";

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

    function _scheduleInvalidate(oLayout) {
        _iResizeRafId = SchedulingRuntime.requestFrameOnce(_iResizeRafId, function () {
            _iResizeRafId = 0;
            if (oLayout && typeof oLayout.invalidate === "function") {
                oLayout.invalidate();
            }
        });
    }

    function _beginResizing() {
        var oDoc = document && document.documentElement;
        ThemeDomRuntime.addClass([oDoc], _RESIZE_CLASS);
        _iResizeEndTimer = SchedulingRuntime.clearTimer(_iResizeEndTimer);
        _iResizeSafetyTimer = SchedulingRuntime.restartTimer(_iResizeSafetyTimer, function () {
            _settleResizing();
        }, _RESIZE_SAFETY_DELAY_MS);
    }

    function _settleResizing() {
        var oDoc = document && document.documentElement;

        _iResizeEndTimer = SchedulingRuntime.clearTimer(_iResizeEndTimer);
        _iResizeSafetyTimer = SchedulingRuntime.clearTimer(_iResizeSafetyTimer);
        ThemeDomRuntime.removeClass([oDoc], _RESIZE_CLASS);
        var oBackgroundRuntime = getBackgroundRuntime();
        if (oBackgroundRuntime && typeof oBackgroundRuntime.onResizeEnd === "function") {
            oBackgroundRuntime.onResizeEnd();
        }
    }

    function _scheduleResizeEnd() {
        _iResizeEndTimer = SchedulingRuntime.restartTimer(_iResizeEndTimer, function () {
            _settleResizing();
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

            _beginResizing();
            var oBackgroundRuntime = getBackgroundRuntime();
            if (oBackgroundRuntime && typeof oBackgroundRuntime.onResizeStart === "function") {
                oBackgroundRuntime.onResizeStart();
            }
            _scheduleInvalidate(oLayout);
            _scheduleResizeEnd();
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
            var that = this;
            _iShellRefreshRafId = SchedulingRuntime.restartFrame(_iShellRefreshRafId, function () {
                _iShellRefreshRafId = 0;
                SchedulingRuntime.nextFrame(function () {
                    that._syncShellFlexAllocation();
                    that._syncShellMetrics();
                    that._syncLayoutViewportGeometry();
                });
            });
        }
    };
});
