sap.ui.define([
    "sap/ui/core/Core",
    "sap/ui/core/Fragment",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerCommandContextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsBuilderRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsYearRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/AnalyticsUiContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsDrilldownRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsExportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsRefreshRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsSelectionBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsLifecycleBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsLoadBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiSemanticConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsStateConstants"
], function (
    Core,
    Fragment,
    AnalyticsFacade,
    ControllerCommandContextRuntime,
    AnalyticsBuilderRuntime,
    AnalyticsYearRuntime,
    AnalyticsContracts,
    AnalyticsUiContracts,
    ModelContracts,
    ControllerViewStateRuntime,
    AnalyticsDrilldownRuntime,
    AnalyticsExportRuntime,
    AnalyticsRefreshRuntime,
    NavigationIntentService,
    AnalyticsSelectionBehavior,
    AnalyticsLifecycleBehavior,
    AnalyticsLoadBehavior,
    UiSemanticConstants,
    AnalyticsStateConstants
) {
    "use strict";

    var PATHS = AnalyticsUiContracts.PATHS;
    var REFRESH_STATE_TASK_KEY = AnalyticsContracts.REFRESH.TASK_KEY;
    var VIEW_MODEL = ModelContracts.MODELS.VIEW;
    var LOAD_REASONS = AnalyticsUiContracts.LOAD_REASONS;
    var VALIDATION_STATES = AnalyticsUiContracts.VALIDATION_STATES;
    var YEAR_PICKER_FIELDS = AnalyticsUiContracts.YEAR_PICKER_FIELDS;

    function buildCtx(oController) {
        return ControllerCommandContextRuntime.buildDefaultCtx(oController);
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

    function getEventSource(oEvent) {
        return oEvent && oEvent.getSource ? oEvent.getSource() : null;
    }

    function getEventParameter(oEvent, sName) {
        return oEvent && oEvent.getParameter ? oEvent.getParameter(sName) : undefined;
    }

    function extractYearValueFromEvent(oEvent) {
        var oSource = getEventSource(oEvent);
        var oSelectedItem = getEventParameter(oEvent, "selectedItem");
        var sSelectedKey = getEventParameter(oEvent, "selectedKey");
        var sValue = getEventParameter(oEvent, "value");

        return AnalyticsYearRuntime.normalizeYearString(
            (oSelectedItem && oSelectedItem.getKey && oSelectedItem.getKey()) ||
            sSelectedKey ||
            sValue ||
            (oSource && oSource.getSelectedKey && oSource.getSelectedKey()) ||
            ""
        );
    }

    function clearCompareYearValidation(fnSetCompareYearValidation) {
        fnSetCompareYearValidation(VALIDATION_STATES.NONE, "");
    }

    function applyYearSelection(oController, sYearPath, sYear, sSelectedYearPath, sCompareYearPath, fnSetCompareYearValidation, fnLoadAnalytics, sReason) {
        if (!sYear) {
            return Promise.resolve();
        }
        ControllerViewStateRuntime.set(oController, sYearPath, sYear);
        ControllerViewStateRuntime.set(oController, PATHS.AVAILABLE_YEARS, AnalyticsYearRuntime.buildYearOptions(oController, sSelectedYearPath, sCompareYearPath));
        if (sYearPath === sSelectedYearPath) {
            AnalyticsYearRuntime.syncCompareYearDefaults(oController, sYear, sSelectedYearPath, sCompareYearPath);
        } else {
            ControllerViewStateRuntime.set(oController, PATHS.COMPARE_YEAR_OPTIONS, AnalyticsYearRuntime.buildCompareYearOptions(oController, sSelectedYearPath, sCompareYearPath));
        }
        clearCompareYearValidation(fnSetCompareYearValidation);
        return fnLoadAnalytics(sReason);
    }

    function openYearPicker(oController, oEvent, sTargetField) {
        var oSource = getEventSource(oEvent);
        AnalyticsYearRuntime.ensureYearPickerRangeForValue(oController, sTargetField);
        return AnalyticsYearRuntime.ensureYearPicker(oController).then(function (oPopover) {
            if (oSource) {
                oPopover.openBy(oSource);
            }
        });
    }

    function queueAnalyticsDrilldown(oController, sFieldName, sFieldValue, mPayload) {
        return AnalyticsDrilldownRuntime.queueAnalyticsDrilldown(oController, sFieldName, sFieldValue, mPayload);
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
            applyAnalyticsFeedValues(oController);
            return aContent[0] || null;
        }).catch(function (oError) {
            oController._pAnalyticsBreakdownsContent = null;
            throw oError;
        });
        return oController._pAnalyticsBreakdownsContent;
    }

    function applyAnalyticsFeedValues(oController) {
        var oValueFeed = oController.byId && oController.byId("analyticsBreakdownsFragment--analyticsComparisonValueFeed");
        var oCategoryFeed = oController.byId && oController.byId("analyticsBreakdownsFragment--analyticsComparisonCategoryFeed");
        if (oValueFeed && typeof oValueFeed.setValues === "function") {
            oValueFeed.setValues([
                getBundleText(oController, "analyticsSelectedYear", [], "Selected year"),
                getBundleText(oController, "analyticsCompareYearLabel", [], "Compare year")
            ]);
        }
        if (oCategoryFeed && typeof oCategoryFeed.setValues === "function") {
            oCategoryFeed.setValues([
                getBundleText(oController, "analyticsMonth", [], "Month")
            ]);
        }
    }

    function onRefreshAnalytics(oController) {
        var oCtx = buildCtx(oController);
        var oRefreshState = ControllerViewStateRuntime.get(oController, PATHS.REFRESH_STATE, {}) || {};
        if (AnalyticsRefreshRuntime.isRefreshQueued(oRefreshState)) {
            ControllerViewStateRuntime.set(oController, PATHS.REFRESH_BUSY, true);
            return oController._pollRefreshStateUntilSettled(AnalyticsRefreshRuntime.REFRESH_POLL_MAX_ATTEMPTS).then(function () {
                return oController._loadAnalytics("pollRefresh");
            }).finally(function () {
                ControllerViewStateRuntime.set(oController, PATHS.REFRESH_BUSY, false);
            });
        }
        AnalyticsRefreshRuntime.invalidatePolls(oController);
        ControllerViewStateRuntime.setMany(oController, {
            [PATHS.REFRESH_BUSY]: true,
            [PATHS.ERROR]: ""
        });
        return Promise.resolve(oCtx && oCtx.analytics && oCtx.analytics.requestRefresh ? oCtx.analytics.requestRefresh({
            requestedBy: AnalyticsContracts.REFRESH.REQUESTED_BY_WEB
        }) : null).then(function (oState) {
            if (oState) {
                ControllerViewStateRuntime.set(oController, PATHS.REFRESH_STATE, oState);
            }
            return oController._pollRefreshStateUntilSettled(AnalyticsRefreshRuntime.REFRESH_POLL_MAX_ATTEMPTS);
        }).then(function () {
            return oController._loadAnalytics("manualRefresh");
        }).catch(function (oError) {
            ControllerViewStateRuntime.set(oController, PATHS.ERROR, String((oError && oError.message) || AnalyticsUiContracts.MESSAGES.ANALYTICS_REFRESH_FAILED));
            throw oError;
        }).finally(function () {
            ControllerViewStateRuntime.set(oController, PATHS.REFRESH_BUSY, false);
        });
    }

    return {
        onInit: function () {
            AnalyticsLifecycleBehavior.onInit(this, new AnalyticsFacade(), REFRESH_STATE_TASK_KEY);
        },

        onAfterRendering: function () {
            AnalyticsLifecycleBehavior.onAfterRendering(this);
            applyAnalyticsFeedValues(this);
        },

        onExit: function () {
            AnalyticsLifecycleBehavior.onExit(this);
        },

        _setCompareYearValidation: function (sState, sText) {
            ControllerViewStateRuntime.setMany(this, {
                [PATHS.COMPARE_YEAR_VALUE_STATE]: sState || VALIDATION_STATES.NONE,
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
            return onRefreshAnalytics(this);
        },

        onSelectAnalyticsYear: function (oEvent) {
            var oSource = getEventSource(oEvent);
            var sYear = extractYearValueFromEvent(oEvent);
            if (!sYear) {
                if (oSource && oSource.setValue) {
                    oSource.setValue(String(ControllerViewStateRuntime.get(this, PATHS.SELECTED_YEAR, "") || ""));
                }
                return Promise.resolve();
            }
            return applyYearSelection(this, PATHS.SELECTED_YEAR, sYear, PATHS.SELECTED_YEAR, PATHS.COMPARE_YEAR, this._setCompareYearValidation.bind(this), this._loadAnalytics.bind(this), LOAD_REASONS.YEAR_CHANGED);
        },

        onLiveChangeAnalyticsYear: function (oEvent) {
            var oInput = getEventSource(oEvent);
            var sValue = AnalyticsYearRuntime.sanitizeYearValue(getEventParameter(oEvent, "value"));
            if (oInput && oInput.getValue && oInput.getValue() !== sValue && oInput.setValue) {
                oInput.setValue(sValue);
            }
        },

        onSelectAnalyticsSource: function (oEvent) {
            var sSource = String(
                getEventParameter(oEvent, "selectedKey") ||
                getEventSource(oEvent) && getEventSource(oEvent).getSelectedKey && getEventSource(oEvent).getSelectedKey() ||
                ""
            ).trim().toUpperCase();
            if (!sSource) {
                return Promise.resolve();
            }
            ControllerViewStateRuntime.set(this, PATHS.SELECTED_SOURCE, sSource);
            return this._loadAnalytics(LOAD_REASONS.SOURCE_CHANGED);
        },

        onChangeAnalyticsCompareYear: function (oEvent) {
            var sStoredCompareYear = String(ControllerViewStateRuntime.get(this, PATHS.COMPARE_YEAR, "") || "").trim();
            return AnalyticsYearRuntime.applyCompareYearChange(this, oEvent, sStoredCompareYear, this._setCompareYearValidation.bind(this), this._loadAnalytics.bind(this));
        },

        onLiveChangeAnalyticsCompareYear: function (oEvent) {
            var oInput = getEventSource(oEvent);
            var sValue = AnalyticsYearRuntime.sanitizeYearValue(getEventParameter(oEvent, "value"));
            if (oInput && oInput.getValue && oInput.getValue() !== sValue && oInput.setValue) {
                oInput.setValue(sValue);
            }
            clearCompareYearValidation(this._setCompareYearValidation.bind(this));
        },

        onOpenAnalyticsSelectedYearPicker: function (oEvent) {
            return openYearPicker(this, oEvent, YEAR_PICKER_FIELDS.SELECTED);
        },

        onOpenAnalyticsCompareYearPicker: function (oEvent) {
            return openYearPicker(this, oEvent, YEAR_PICKER_FIELDS.COMPARE);
        },

        onNavigateAnalyticsYearPickerBack: function () {
            var sTargetField = String(ControllerViewStateRuntime.get(this, PATHS.YEAR_PICKER_TARGET_FIELD, YEAR_PICKER_FIELDS.SELECTED) || YEAR_PICKER_FIELDS.SELECTED);
            var iRangeStart = Number(ControllerViewStateRuntime.get(this, PATHS.YEAR_PICKER_RANGE_START, new Date().getFullYear() - 9) || 0);
            AnalyticsYearRuntime.syncYearPickerState(this, sTargetField, iRangeStart - 20);
        },

        onNavigateAnalyticsYearPickerForward: function () {
            var sTargetField = String(ControllerViewStateRuntime.get(this, PATHS.YEAR_PICKER_TARGET_FIELD, YEAR_PICKER_FIELDS.SELECTED) || YEAR_PICKER_FIELDS.SELECTED);
            var iRangeStart = Number(ControllerViewStateRuntime.get(this, PATHS.YEAR_PICKER_RANGE_START, new Date().getFullYear() - 9) || 0);
            AnalyticsYearRuntime.syncYearPickerState(this, sTargetField, iRangeStart + 20);
        },

        onSelectAnalyticsYearFromPicker: function (oEvent) {
            var sTargetField = String(ControllerViewStateRuntime.get(this, PATHS.YEAR_PICKER_TARGET_FIELD, YEAR_PICKER_FIELDS.SELECTED) || YEAR_PICKER_FIELDS.SELECTED);
            var iRangeStart = Number(ControllerViewStateRuntime.get(this, PATHS.YEAR_PICKER_RANGE_START, new Date().getFullYear() - 9) || 0);
            var oSource = getEventSource(oEvent);
            var sYear = AnalyticsYearRuntime.normalizeYearString(oSource && oSource.data && oSource.data("year"));
            var sTargetPath = sTargetField === YEAR_PICKER_FIELDS.COMPARE ? PATHS.COMPARE_YEAR : PATHS.SELECTED_YEAR;
            var sReason = sTargetField === YEAR_PICKER_FIELDS.COMPARE ? LOAD_REASONS.COMPARE_YEAR_PICKED : LOAD_REASONS.YEAR_PICKED;
            if (!sYear) {
                return Promise.resolve();
            }
            AnalyticsYearRuntime.syncYearPickerState(this, sTargetField, iRangeStart);
            if (this._oAnalyticsYearPicker) {
                this._oAnalyticsYearPicker.close();
            }
            return applyYearSelection(this, sTargetPath, sYear, PATHS.SELECTED_YEAR, PATHS.COMPARE_YEAR, this._setCompareYearValidation.bind(this), this._loadAnalytics.bind(this), sReason);
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
            var oSource = getEventSource(oEvent);
            var sPreset = String((oSource && oSource.data && oSource.data("preset")) || "").trim().toUpperCase();
            if (!sPreset) {
                return Promise.resolve();
            }
            return AnalyticsYearRuntime.applyYearPreset(this, sPreset, PATHS.SELECTED_YEAR, PATHS.COMPARE_YEAR, this._loadAnalytics.bind(this));
        },

        onDrilldownAnalyticsBuilder: function (oEvent) {
            var oViewModel = this.getModel && this.getModel(VIEW_MODEL);
            var sBuilderDimension = String(oViewModel && oViewModel.getProperty && oViewModel.getProperty(PATHS.BUILDER_DIMENSION) || AnalyticsContracts.BUILDER.FALLBACK_DIMENSION).trim().toUpperCase();
            var sBuilderMetric = String(oViewModel && oViewModel.getProperty && oViewModel.getProperty(PATHS.BUILDER_METRIC) || "").trim().toUpperCase();
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            var sLabel = String((oPoint && (oPoint.Dimension || oPoint.label || oPoint.labelShort)) || "").trim();
            var mMap = {
                MONTH: "DateCheck",
                LPC: "Lpc",
                PROFESSION: "ProfessionText",
                LOCATION: "LocationKey",
                SOURCE: "SourceKey"
            };
            return queueAnalyticsDrilldown(this, mMap[sBuilderDimension], sLabel, {
                dimension: sBuilderDimension,
                metric: sBuilderMetric,
                monthLabel: sBuilderDimension === AnalyticsContracts.DIMENSIONS.MONTH ? sLabel : ""
            });
        },

        onDrilldownAnalyticsSource: function (oEvent) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            var sSource = String((oPoint && oPoint.Source) || "").trim().toUpperCase();
            if (!sSource || sSource === "ALL") {
                return Promise.resolve(false);
            }
            return queueAnalyticsDrilldown(this, "SourceKey", sSource, {
                dimension: AnalyticsContracts.DIMENSIONS.SOURCE,
                metric: String(AnalyticsContracts.METRICS.TOTAL || oPoint && oPoint.metric || "").trim().toUpperCase()
            });
        },

        onDrilldownAnalyticsSourceFailedChecks: function (oEvent) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            var sSource = String((oPoint && oPoint.Source) || "").trim().toUpperCase();
            if (!sSource || sSource === "ALL") {
                return Promise.resolve(false);
            }
            return queueAnalyticsDrilldown(this, "SourceKey", sSource, {
                dimension: AnalyticsContracts.DIMENSIONS.SOURCE,
                metric: String(AnalyticsContracts.METRICS.FAILED_CHECKS).trim().toUpperCase()
            });
        },

        onDrilldownAnalyticsSourceFailedBarriers: function (oEvent) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            var sSource = String((oPoint && oPoint.Source) || "").trim().toUpperCase();
            if (!sSource || sSource === "ALL") {
                return Promise.resolve(false);
            }
            return queueAnalyticsDrilldown(this, "SourceKey", sSource, {
                dimension: AnalyticsContracts.DIMENSIONS.SOURCE,
                metric: String(AnalyticsContracts.METRICS.FAILED_BARRIERS).trim().toUpperCase()
            });
        },

        onDrilldownAnalyticsProfession: function (oEvent) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            return queueAnalyticsDrilldown(this, "ProfessionText", String((oPoint && oPoint.Profession) || "").trim(), {
                dimension: AnalyticsContracts.DIMENSIONS.PROFESSION,
                metric: String(AnalyticsContracts.METRICS.FAILED_CHECKS).trim().toUpperCase()
            });
        },

        onDrilldownAnalyticsLpc: function (oEvent) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            return queueAnalyticsDrilldown(this, "Lpc", String((oPoint && oPoint.LPC) || "").trim(), {
                dimension: AnalyticsContracts.DIMENSIONS.LPC,
                metric: String(AnalyticsContracts.METRICS.FAILED_BARRIERS).trim().toUpperCase()
            });
        },

        onDrilldownAnalyticsLocation: function (oEvent) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            return queueAnalyticsDrilldown(this, "LocationKey", String((oPoint && oPoint.Location) || "").trim(), {
                dimension: AnalyticsContracts.DIMENSIONS.LOCATION,
                metric: String(AnalyticsContracts.METRICS.FAILED_CHECKS).trim().toUpperCase()
            });
        },

        onOpenAnalyticsReportDialog: function () {
            return AnalyticsExportRuntime.ensureAnalyticsReportDialog(this).then(function (oDialog) {
                oDialog.open();
            });
        },

        onCloseAnalyticsReportDialog: function () {
            if (this._oAnalyticsReportDialog) {
                this._oAnalyticsReportDialog.close();
            }
        },

        onExportAnalyticsReport: function () {
            return AnalyticsExportRuntime.exportAnalyticsReport(this);
        },

        onCloseAnalytics: function () {
            NavigationIntentService.navigateBackFromAnalytics(this);
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
            if (sNormalizedStatus === AnalyticsStateConstants.REFRESH_STATUS.ERROR) {
                return UiSemanticConstants.OBJECT_STATUS_STATE.ERROR;
            }
            if (bIsRunning) {
                return UiSemanticConstants.OBJECT_STATUS_STATE.WARNING;
            }
            if (sNormalizedStatus === AnalyticsStateConstants.REFRESH_STATUS.SUCCESS || sNormalizedStatus === AnalyticsStateConstants.REFRESH_STATUS.READY) {
                return UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS;
            }
            return UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION;
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
            return !bRefreshBusy && !bIsRunning && sStatus !== AnalyticsStateConstants.REFRESH_STATUS.REQUESTED;
        },

        formatAnalyticsInfoMessageType: function () {
            return UiSemanticConstants.MESSAGE_TYPE.INFORMATION;
        },

        formatAnalyticsWarningMessageType: function () {
            return UiSemanticConstants.MESSAGE_TYPE.WARNING;
        },

        formatAnalyticsCompareYearState: function () {
            return UiSemanticConstants.OBJECT_STATUS_STATE.NONE;
        },

        formatAnalyticsSelectedYearState: function () {
            return UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS;
        },

        formatAnalyticsSourceState: function () {
            return UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION;
        },

        formatRefreshMessageType: function (oRefreshState) {
            return coerceText(oRefreshState && oRefreshState.lastError)
                ? AnalyticsStateConstants.REFRESH_MESSAGE_TYPE.ERROR
                : AnalyticsStateConstants.REFRESH_MESSAGE_TYPE.ACTIVE;
        },

        formatRefreshMessageVisible: function (oRefreshState) {
            var sStatus = coerceText(oRefreshState && oRefreshState.status).toUpperCase();
            return sStatus === AnalyticsStateConstants.REFRESH_STATUS.REQUESTED ||
                !!(oRefreshState && oRefreshState.isRunning) ||
                !!coerceText(oRefreshState && oRefreshState.lastError);
        }
    };
});
