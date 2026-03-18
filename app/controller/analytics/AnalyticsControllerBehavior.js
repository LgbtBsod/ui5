sap.ui.define([
    "sap/ui/core/Core",
    "sap/ui/core/Fragment",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsBuilderRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/AnalyticsUiContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsYearBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsDrilldownBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsSelectionBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsRefreshBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsLifecycleBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsLoadBehavior"
], function (
    Core,
    Fragment,
    AnalyticsFacade,
    CtxFactory,
    AnalyticsBuilderRuntime,
    AnalyticsContracts,
    AnalyticsUiContracts,
    ModelContracts,
    ControllerViewStateRuntime,
    AnalyticsYearBehavior,
    AnalyticsDrilldownBehavior,
    AnalyticsSelectionBehavior,
    AnalyticsRefreshBehavior,
    AnalyticsLifecycleBehavior,
    AnalyticsLoadBehavior
) {
    "use strict";

    var PATHS = AnalyticsUiContracts.PATHS;
    var REFRESH_STATE_TASK_KEY = AnalyticsContracts.REFRESH.TASK_KEY;
    var VIEW_MODEL = ModelContracts.MODELS.VIEW;

    function buildCtx(oController) {
        return CtxFactory.buildCtx(oController, {});
    }

    function coerceText(vValue) {
        return String(vValue || "").trim();
    }

    function getBundleText(oController, sKey, aArgs, sFallback) {
        var oBundle = oController && oController.getResourceBundle && oController.getResourceBundle();
        try {
            if (oBundle && oBundle.getText) {
                return String(oBundle.getText(sKey, aArgs || []) || sFallback || sKey);
            }
        } catch (_bundleError) {
            // Fall back to the provided static text below.
        }
        return String(sFallback || sKey || "");
    }

    function ensureVizContentLoaded(oController) {
        var oHost = oController.byId && oController.byId("analyticsBreakdownsHost");
        if (!oHost) {
            return Promise.resolve(null);
        }
        if (oController._pAnalyticsBreakdownsContent) {
            return oController._pAnalyticsBreakdownsContent;
        }
        oController._pAnalyticsBreakdownsContent = Core.loadLibrary("sap.viz", { async: true }).then(function () {
            return Fragment.load({
                id: oController.getView().createId("analyticsBreakdownsFragment"),
                name: "PRODUCTION_CONTROL_CHECKLIST.views.fragment.WorkflowAnalyticsBreakdowns",
                controller: oController
            });
        }).then(function (oFragment) {
            var aContent = Array.isArray(oFragment) ? oFragment : [oFragment];
            oHost.removeAllItems();
            aContent.forEach(function (oContent) {
                if (oContent) {
                    oHost.addItem(oContent);
                }
            });
            return aContent[0] || null;
        }).catch(function (oError) {
            oController._pAnalyticsBreakdownsContent = null;
            throw oError;
        });
        return oController._pAnalyticsBreakdownsContent;
    }

    return {
        onInit: function () {
            AnalyticsLifecycleBehavior.onInit(this, new AnalyticsFacade(), REFRESH_STATE_TASK_KEY);
        },

        onAfterRendering: function () {
            AnalyticsLifecycleBehavior.onAfterRendering(this);
        },

        onExit: function () {
            AnalyticsLifecycleBehavior.onExit(this);
        },

        _setCompareYearValidation: function (sState, sText) {
            ControllerViewStateRuntime.setMany(this, {
                [PATHS.COMPARE_YEAR_VALUE_STATE]: sState || AnalyticsUiContracts.VALIDATION_STATES.NONE,
                [PATHS.COMPARE_YEAR_VALUE_STATE_TEXT]: sText || ""
            });
        },

        _loadAnalytics: function (sReason) {
            return AnalyticsLoadBehavior.loadAnalytics(this, sReason, PATHS.SELECTED_YEAR, PATHS.COMPARE_YEAR, buildCtx);
        },

        _pollRefreshStateUntilSettled: function (iAttemptsLeft) {
            return AnalyticsLoadBehavior.pollRefreshStateUntilSettled(this, iAttemptsLeft, buildCtx);
        },

        _onAnalyticsMatched: function () {
            AnalyticsLifecycleBehavior.onRouteEnter(this);
            return ensureVizContentLoaded(this).then(function () {
                return AnalyticsLoadBehavior.onAnalyticsMatched(this, function (oController, sReason) {
                    return oController._loadAnalytics(sReason);
                });
            }.bind(this)).catch(function (oError) {
                ControllerViewStateRuntime.set(this, PATHS.ERROR, String((oError && oError.message) || AnalyticsUiContracts.MESSAGES.ANALYTICS_LOAD_FAILED));
                return null;
            }.bind(this));
        },

        _onAnalyticsRouteLeave: function () {
            AnalyticsLifecycleBehavior.onRouteLeave(this);
        },

        _startAnalyticsRefreshTimer: function () {
            AnalyticsLifecycleBehavior.startRefreshTimer(this);
        },

        onRefreshAnalytics: function () {
            return AnalyticsRefreshBehavior.onRefreshAnalytics(this, buildCtx, function (oController, iAttemptsLeft) {
                return oController._pollRefreshStateUntilSettled(iAttemptsLeft);
            }, function (oController, sReason) {
                return oController._loadAnalytics(sReason);
            });
        },

        onSelectAnalyticsYear: function (oEvent) {
            return AnalyticsYearBehavior.onSelectAnalyticsYear(this, oEvent, PATHS.SELECTED_YEAR, PATHS.COMPARE_YEAR, this._setCompareYearValidation.bind(this), this._loadAnalytics.bind(this));
        },

        onLiveChangeAnalyticsYear: function (oEvent) {
            AnalyticsYearBehavior.onLiveChangeAnalyticsYear(oEvent);
        },

        onSelectAnalyticsSource: function (oEvent) {
            return AnalyticsYearBehavior.onSelectAnalyticsSource(this, oEvent, this._loadAnalytics.bind(this));
        },

        onChangeAnalyticsCompareYear: function (oEvent) {
            return AnalyticsYearBehavior.onChangeAnalyticsCompareYear(this, oEvent, this._setCompareYearValidation.bind(this), this._loadAnalytics.bind(this));
        },

        onLiveChangeAnalyticsCompareYear: function (oEvent) {
            AnalyticsYearBehavior.onLiveChangeAnalyticsCompareYear(this, oEvent, this._setCompareYearValidation.bind(this));
        },

        onOpenAnalyticsSelectedYearPicker: function (oEvent) {
            return AnalyticsYearBehavior.onOpenAnalyticsSelectedYearPicker(this, oEvent);
        },

        onOpenAnalyticsCompareYearPicker: function (oEvent) {
            return AnalyticsYearBehavior.onOpenAnalyticsCompareYearPicker(this, oEvent);
        },

        onNavigateAnalyticsYearPickerBack: function () {
            AnalyticsYearBehavior.onNavigateAnalyticsYearPickerBack(this);
        },

        onNavigateAnalyticsYearPickerForward: function () {
            AnalyticsYearBehavior.onNavigateAnalyticsYearPickerForward(this);
        },

        onSelectAnalyticsYearFromPicker: function (oEvent) {
            return AnalyticsYearBehavior.onSelectAnalyticsYearFromPicker(this, oEvent, PATHS.SELECTED_YEAR, PATHS.COMPARE_YEAR, this._setCompareYearValidation.bind(this), this._loadAnalytics.bind(this));
        },

        onSelectAnalyticsMetric: function (oEvent) {
            return AnalyticsSelectionBehavior.onSelectAnalyticsMetric(this, oEvent, function (oController) {
                AnalyticsBuilderRuntime.applyComparisonMetricSelection(oController);
            });
        },

        onSelectAnalyticsBuilderDimension: function (oEvent) {
            return AnalyticsSelectionBehavior.onSelectAnalyticsBuilderDimension(this, oEvent, function (oController, mOverrides) {
                AnalyticsBuilderRuntime.applyBuilderSelection(oController, mOverrides);
            });
        },

        onSelectAnalyticsBuilderMetric: function (oEvent) {
            return AnalyticsSelectionBehavior.onSelectAnalyticsBuilderMetric(this, oEvent, function (oController, mOverrides) {
                AnalyticsBuilderRuntime.applyBuilderSelection(oController, mOverrides);
            });
        },

        onApplyAnalyticsYearPreset: function (oEvent) {
            return AnalyticsYearBehavior.onApplyAnalyticsYearPreset(this, oEvent, PATHS.SELECTED_YEAR, PATHS.COMPARE_YEAR, this._loadAnalytics.bind(this));
        },

        onDrilldownAnalyticsBuilder: function (oEvent) {
            var oViewModel = this.getModel && this.getModel(VIEW_MODEL);
            var sBuilderDimension = String(oViewModel && oViewModel.getProperty && oViewModel.getProperty(PATHS.BUILDER_DIMENSION) || AnalyticsContracts.BUILDER.FALLBACK_DIMENSION).trim().toUpperCase();
            var sBuilderMetric = String(oViewModel && oViewModel.getProperty && oViewModel.getProperty(PATHS.BUILDER_METRIC) || "").trim().toUpperCase();
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsBuilder(this, oEvent, sBuilderDimension, sBuilderMetric);
        },

        onDrilldownAnalyticsSource: function (oEvent) {
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsSource(this, oEvent, AnalyticsContracts.METRICS.TOTAL);
        },

        onDrilldownAnalyticsSourceFailedChecks: function (oEvent) {
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsSource(this, oEvent, AnalyticsContracts.METRICS.FAILED_CHECKS);
        },

        onDrilldownAnalyticsSourceFailedBarriers: function (oEvent) {
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsSource(this, oEvent, AnalyticsContracts.METRICS.FAILED_BARRIERS);
        },

        onDrilldownAnalyticsProfession: function (oEvent) {
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsProfession(this, oEvent, AnalyticsContracts.METRICS.FAILED_CHECKS);
        },

        onDrilldownAnalyticsLpc: function (oEvent) {
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsLpc(this, oEvent, AnalyticsContracts.METRICS.FAILED_BARRIERS);
        },

        onDrilldownAnalyticsLocation: function (oEvent) {
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsLocation(this, oEvent, AnalyticsContracts.METRICS.FAILED_CHECKS);
        },

        onOpenAnalyticsReportDialog: function () {
            return AnalyticsDrilldownBehavior.onOpenAnalyticsReportDialog(this);
        },

        onCloseAnalyticsReportDialog: function () {
            AnalyticsDrilldownBehavior.onCloseAnalyticsReportDialog(this);
        },

        onExportAnalyticsReport: function () {
            return AnalyticsDrilldownBehavior.onExportAnalyticsReport(this);
        },

        onCloseAnalytics: function () {
            AnalyticsDrilldownBehavior.onCloseAnalytics(this);
        },

        formatAnalyticsMatrixMetricLabel: function (sMetricKey) {
            var sKey = coerceText(sMetricKey).toUpperCase();

            if (sKey === AnalyticsContracts.METRICS.TOTAL) {
                return getBundleText(this, "analyticsMetricTotal", [], "Total");
            }
            if (sKey === AnalyticsContracts.METRICS.FAILED_CHECKS) {
                return getBundleText(this, "analyticsMetricFailedChecks", [], "Failed checks");
            }
            if (sKey === AnalyticsContracts.METRICS.FAILED_BARRIERS) {
                return getBundleText(this, "analyticsMetricFailedBarriers", [], "Failed barriers");
            }
            if (sKey === AnalyticsContracts.METRICS.FAILED_CHECKLISTS) {
                return getBundleText(this, "analyticsMetricFailedChecklistCount", [], "Failed checklists");
            }
            if (sKey === AnalyticsContracts.METRICS.FAILED_BARRIER_CHECKLISTS) {
                return getBundleText(this, "analyticsMetricFailedBarrierChecklistCount", [], "Failed barrier checklists");
            }
            return sMetricKey;
        },

        formatAnalyticsSourceContext: function (sSelectedSource) {
            var sSourceKey = coerceText(sSelectedSource).toUpperCase();
            var sResolvedSourceText = getBundleText(this, "analyticsSourceAll", [], "All");

            if (sSourceKey === AnalyticsContracts.SOURCES.WEB) {
                sResolvedSourceText = getBundleText(this, "analyticsSourceWeb", [], "Web");
            } else if (sSourceKey === AnalyticsContracts.SOURCES.INTEGRATION) {
                sResolvedSourceText = getBundleText(this, "analyticsSourceIntegration", [], "Integration");
            }
            return getBundleText(this, "analyticsSourceFilterLabel", [], "Source") + ": " + sResolvedSourceText;
        },

        formatRefreshStatusState: function (oRefreshState) {
            var sNormalizedStatus = coerceText(oRefreshState && oRefreshState.status).toUpperCase();
            var bIsRunning = !!(oRefreshState && oRefreshState.isRunning);
            if (sNormalizedStatus === "ERROR") {
                return "Error";
            }
            if (bIsRunning) {
                return "Warning";
            }
            if (sNormalizedStatus === "SUCCESS" || sNormalizedStatus === "READY") {
                return "Success";
            }
            return "Information";
        },

        formatRefreshStatusText: function (oRefreshState) {
            var sStatus = coerceText(oRefreshState && oRefreshState.status);
            var sMessage = coerceText(oRefreshState && (oRefreshState.lastMessage || oRefreshState.lastError));
            if (sMessage) {
                return sMessage;
            }
            if (!sStatus) {
                return getBundleText(this, "analyticsRefreshIdle", [], "Idle");
            }
            return sStatus;
        },

        formatRefreshEnabled: function (bRefreshBusy, oRefreshState) {
            var sStatus = coerceText(oRefreshState && oRefreshState.status).toUpperCase();
            var bIsRunning = !!(oRefreshState && oRefreshState.isRunning);
            return !bRefreshBusy && !bIsRunning && sStatus !== "REQUESTED";
        }
    };
});
