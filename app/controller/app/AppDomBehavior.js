sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ShellGlobalsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Ui5RuntimeFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/AppShellDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SemanticDomRuntime"
], function (ModelStateRuntime, ThemeDomRuntime, SchedulingRuntime, ModelContracts, ShellGlobalsRuntime, Ui5RuntimeFacade, AppShellDomRuntime, SemanticDomRuntime) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var SHELL_MODEL = MODELS.SHELL;
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

    function getGlobalDomNodes() {
        var oNodes = ThemeDomRuntime.getNodes();
        return {
            root: oNodes.root || null,
            body: oNodes.body || null,
            container: oNodes.container || null
        };
    }

    function getAppContainerDom(oController) {
        var oRoot = oController && oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
        if (oRoot && oRoot.closest) {
            return oRoot.closest(".chkAppRoot") || oRoot;
        }
        return oRoot || null;
    }

    function getAppDomTargets(oController) {
        var oNodes = getGlobalDomNodes();
        var oContainer = getAppContainerDom(oController);
        var oAppDom = oController && oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();

        return [oNodes.root, oNodes.body, oContainer, oAppDom];
    }

    function syncSemanticAttributes(oController) {
        var oMainHost = oController && oController.byId && oController.byId("mainContentHost");
        var oFeedbackRegion = oController && oController.byId && oController.byId("feedbackCorrelationRegion");

        SemanticDomRuntime.syncAttributes(oMainHost, { role: "main" });
        SemanticDomRuntime.syncAttributes(oFeedbackRegion, { role: "region" });
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
        var oDoc = getGlobalDomNodes().root;
        ThemeDomRuntime.addClass([oDoc], _RESIZE_CLASS);
        oRuntimeState.resizeEndTimer = SchedulingRuntime.clearTimer(oRuntimeState.resizeEndTimer);
        oRuntimeState.resizeSafetyTimer = SchedulingRuntime.restartTimer(oRuntimeState.resizeSafetyTimer, function () {
            _settleResizing(oController);
        }, _RESIZE_SAFETY_DELAY_MS);
    }

    function _settleResizing(oController) {
        var oRuntimeState = getDomRuntimeState(oController);
        var oDoc = getGlobalDomNodes().root;

        oRuntimeState.resizeEndTimer = SchedulingRuntime.clearTimer(oRuntimeState.resizeEndTimer);
        oRuntimeState.resizeSafetyTimer = SchedulingRuntime.clearTimer(oRuntimeState.resizeSafetyTimer);
        ThemeDomRuntime.removeClass([oDoc], _RESIZE_CLASS);
        AppShellDomRuntime.notifyBackgroundResize(getBackgroundRuntime(), "end");
    }

    function _scheduleResizeEnd(oController) {
        var oRuntimeState = getDomRuntimeState(oController);
        oRuntimeState.resizeEndTimer = SchedulingRuntime.restartTimer(oRuntimeState.resizeEndTimer, function () {
            _settleResizing(oController);
        }, _RESIZE_END_DELAY_MS);
    }

    return {
        onSkipToMainContent: function (oEvent) {
            AppShellDomRuntime.focusMainContent(this, oEvent);
        },

        _syncShellFlexAllocation: function () {
            AppShellDomRuntime.syncShellFlexItem(this);
        },

        _syncLayoutViewportGeometry: function () {
            var oLayout = this.byId("mainFcl");

            _beginResizing(this);
            AppShellDomRuntime.notifyBackgroundResize(getBackgroundRuntime(), "start");
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
            var bCompact = !!ModelStateRuntime.read(this, SHELL_MODEL, MODEL_PATHS.SHELL_COMPACT_DENSITY, false);
            ThemeDomRuntime.toggleClass(getAppDomTargets(this), "appDensityCompact", bCompact);
        },

        _applyInvertedBlockSchemeClass: function () {
            var bEnabled = !!ModelStateRuntime.read(this, SHELL_MODEL, MODEL_PATHS.SHELL_INVERTED_BLOCK_SCHEME, false);
            ThemeDomRuntime.toggleClass(getAppDomTargets(this), "appInvertedBlockScheme", bEnabled);
        },

        _syncShellMetrics: function () {
            AppShellDomRuntime.syncShellMetricVars(this, getGlobalDomNodes().root);
        },

        _syncSemanticAttributes: function () {
            syncSemanticAttributes(this);
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
