sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/Base.controller",
    "sap/ui/core/Fragment",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsBuilderRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsYearRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsDrilldownRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsFormatRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsExportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsRefreshRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/navigation/WorkspaceRouteNavigation",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsLifecycleBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiSemanticConstants"
], function (
    BaseController,
    Fragment,
    ControllerTextRuntime,
    AnalyticsFacade,
    AnalyticsBuilderRuntime,
    AnalyticsYearRuntime,
    AnalyticsContracts,
    ModelContracts,
    ControllerViewStateRuntime,
    AnalyticsDrilldownRuntime,
    AnalyticsFormatRuntime,
    AnalyticsExportRuntime,
    AnalyticsRefreshRuntime,
    WorkspaceRouteNavigation,
    AnalyticsLifecycleBehavior,
    AnalyticsLoadRuntime,
    UiSemanticConstants
) {
    "use strict";

    var PATHS = AnalyticsContracts.PATHS;
    var REFRESH_STATE_TASK_KEY = AnalyticsContracts.REFRESH.TASK_KEY;
    var VIEW_MODEL = ModelContracts.MODELS.VIEW;
    var LOAD_REASONS = AnalyticsContracts.LOAD_REASONS;
    var VALIDATION_STATES = AnalyticsContracts.VALIDATION_STATES;
    var YEAR_PICKER_FIELDS = AnalyticsContracts.YEAR_PICKER_FIELDS;

    function buildCtx(oController) {
        return oController && typeof oController._ctx === "function" ? oController._ctx() : {};
    }

    function getBundleText(oController, sKey, aArgs, sFallback) {
        return ControllerTextRuntime.getText(oController, sKey, aArgs || [], sFallback || "");
    }

    function extractYearValueFromEvent(oEvent) {
        var oSource = AnalyticsDrilldownRuntime.getEventSource(oEvent);
        var oSelectedItem = AnalyticsDrilldownRuntime.getEventParameter(oEvent, "selectedItem");
        var sSelectedKey = AnalyticsDrilldownRuntime.getEventParameter(oEvent, "selectedKey");
        var sValue = AnalyticsDrilldownRuntime.getEventParameter(oEvent, "value");
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
        var oSource = AnalyticsDrilldownRuntime.getEventSource(oEvent);
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

    function onSelectAnalyticsMetric(oController, oEvent) {
        var sMetric = AnalyticsBuilderRuntime.getSelectedKeyFromEvent(oEvent);
        if (!sMetric) {
            return;
        }
        ControllerViewStateRuntime.set(oController, PATHS.COMPARISON_METRIC, sMetric);
        AnalyticsBuilderRuntime.applyComparisonMetricSelection(oController);
    }

    function onSelectAnalyticsBuilderDimension(oController, oEvent) {
        var sDimension = AnalyticsBuilderRuntime.getSelectedKeyFromEvent(oEvent);
        if (!sDimension) {
            return;
        }
        AnalyticsBuilderRuntime.applyBuilderSelection(oController, { dimension: sDimension });
    }

    function onSelectAnalyticsBuilderMetric(oController, oEvent) {
        var sMetric = AnalyticsBuilderRuntime.getSelectedKeyFromEvent(oEvent);
        if (!sMetric) {
            return;
        }
        AnalyticsBuilderRuntime.applyBuilderSelection(oController, { metric: sMetric });
    }

    function ensureVizContentLoaded(oController) {
        var oHost = oController.byId && oController.byId("analyticsBreakdownsHost");
        if (!oHost) {
            return Promise.resolve(null);
        }
        if (oController._pAnalyticsBreakdownsContent) {
            return oController._pAnalyticsBreakdownsContent;
        }
        oController._pAnalyticsBreakdownsContent = new Promise(function (resolve, reject) {
            sap.ui.require([
                "sap/viz/ui5/controls/VizFrame",
                "sap/viz/ui5/controls/Popover"
            ], function () {
                resolve(Fragment.load({
                    id: oController.getView().createId("analyticsBreakdownsFragment"),
                    name: "PRODUCTION_CONTROL_CHECKLIST.views.fragment.WorkflowAnalyticsBreakdowns",
                    controller: oController
                }));
            }, reject);
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
                getBundleText(oController, "analyticsSelectedYear"),
                getBundleText(oController, "analyticsCompareYearLabel")
            ]);
        }
        if (oCategoryFeed && typeof oCategoryFeed.setValues === "function") {
            oCategoryFeed.setValues([
                getBundleText(oController, "analyticsMonth")
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
            ControllerViewStateRuntime.set(oController, PATHS.ERROR, String((oError && oError.message) || AnalyticsContracts.MESSAGES.ANALYTICS_REFRESH_FAILED));
            throw oError;
        }).finally(function () {
            ControllerViewStateRuntime.set(oController, PATHS.REFRESH_BUSY, false);
        });
    }

    function loadAnalyticsBehavior(oController, sReason, fnBuildCtx) {
        return AnalyticsLoadRuntime.loadAnalytics(oController, sReason, {
            applyBuilderSelection: function (oTarget) {
                AnalyticsBuilderRuntime.applyBuilderSelection(oTarget);
            },
            applyComparisonMetricSelection: function (oTarget) {
                AnalyticsBuilderRuntime.applyComparisonMetricSelection(oTarget);
            },
            buildCompareYearOptions: function (oTarget) {
                return AnalyticsYearRuntime.buildCompareYearOptions(oTarget, PATHS.SELECTED_YEAR, PATHS.COMPARE_YEAR);
            },
            buildCtx: fnBuildCtx,
            buildYearOptions: function (oTarget) {
                return AnalyticsYearRuntime.buildYearOptions(oTarget, PATHS.SELECTED_YEAR, PATHS.COMPARE_YEAR);
            },
            setCompareYearValidation: function (oTarget, sState, sText) {
                oTarget._setCompareYearValidation(sState, sText);
            },
            syncAnalyticsContextHints: function (oTarget) {
                AnalyticsBuilderRuntime.syncAnalyticsContextHints(oTarget);
            },
            syncCompareYearDefaults: function (oTarget, sSelectedYear) {
                return AnalyticsYearRuntime.syncCompareYearDefaults(oTarget, sSelectedYear, PATHS.SELECTED_YEAR, PATHS.COMPARE_YEAR);
            }
        });
    }

    function pollRefreshStateUntilSettledBehavior(oController, iAttemptsLeft, fnBuildCtx) {
        var oCtx = fnBuildCtx(oController);
        return AnalyticsRefreshRuntime.pollRefreshStateUntilSettled(oController, iAttemptsLeft, function () {
            return oCtx && oCtx.analytics && oCtx.analytics.fetchRefreshState ? oCtx.analytics.fetchRefreshState() : null;
        });
    }

    function onAnalyticsMatchedBehavior(oController, fnLoadAnalytics) {
        oController._bAnalyticsInitialRouteHandled = true;
        return fnLoadAnalytics(oController, "routeMatched").then(function (vResult) {
            if (oController._bAnalyticsRouteActive && typeof oController._startAnalyticsRefreshTimer === "function") {
                oController._startAnalyticsRefreshTimer();
            }
            return vResult;
        });
    }

    return BaseController.extend("PRODUCTION_CONTROL_CHECKLIST.controller.Analytics", {
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
            return loadAnalyticsBehavior(this, sReason, buildCtx);
        },
        _pollRefreshStateUntilSettled: function (iAttemptsLeft) {
            return pollRefreshStateUntilSettledBehavior(this, iAttemptsLeft, buildCtx);
        },
        _onAnalyticsMatched: function () {
            AnalyticsLifecycleBehavior.onRouteEnter(this);
            return ensureVizContentLoaded(this).then(function () {
                return onAnalyticsMatchedBehavior(this, function (oController, sReason) {
                    return oController._loadAnalytics(sReason);
                });
            }.bind(this)).catch(function (oError) {
                ControllerViewStateRuntime.set(this, PATHS.ERROR, String((oError && oError.message) || AnalyticsContracts.MESSAGES.ANALYTICS_LOAD_FAILED));
                return null;
            }.bind(this));
        },
        _onAnalyticsRouteLeave: function () { AnalyticsLifecycleBehavior.onRouteLeave(this); },
        _startAnalyticsRefreshTimer: function () { AnalyticsLifecycleBehavior.startRefreshTimer(this); },
        onRefreshAnalytics: function () { return onRefreshAnalytics(this); },
        onSelectAnalyticsYear: function (oEvent) {
            var oSource = AnalyticsDrilldownRuntime.getEventSource(oEvent);
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
            var oInput = AnalyticsDrilldownRuntime.getEventSource(oEvent);
            var sValue = AnalyticsYearRuntime.sanitizeYearValue(AnalyticsDrilldownRuntime.getEventParameter(oEvent, "value"));
            if (oInput && oInput.getValue && oInput.getValue() !== sValue && oInput.setValue) {
                oInput.setValue(sValue);
            }
        },
        onSelectAnalyticsSource: function (oEvent) {
            var sSource = AnalyticsDrilldownRuntime.resolveSelectedSource(oEvent);
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
            var oInput = AnalyticsDrilldownRuntime.getEventSource(oEvent);
            var sValue = AnalyticsYearRuntime.sanitizeYearValue(AnalyticsDrilldownRuntime.getEventParameter(oEvent, "value"));
            if (oInput && oInput.getValue && oInput.getValue() !== sValue && oInput.setValue) {
                oInput.setValue(sValue);
            }
            clearCompareYearValidation(this._setCompareYearValidation.bind(this));
        },
        onOpenAnalyticsSelectedYearPicker: function (oEvent) { return openYearPicker(this, oEvent, YEAR_PICKER_FIELDS.SELECTED); },
        onOpenAnalyticsCompareYearPicker: function (oEvent) { return openYearPicker(this, oEvent, YEAR_PICKER_FIELDS.COMPARE); },
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
            var oSource = AnalyticsDrilldownRuntime.getEventSource(oEvent);
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
        onSelectAnalyticsMetric: function (oEvent) { return onSelectAnalyticsMetric(this, oEvent); },
        onSelectAnalyticsBuilderDimension: function (oEvent) { return onSelectAnalyticsBuilderDimension(this, oEvent); },
        onSelectAnalyticsBuilderMetric: function (oEvent) { return onSelectAnalyticsBuilderMetric(this, oEvent); },
        onApplyAnalyticsYearPreset: function (oEvent) {
            var oSource = AnalyticsDrilldownRuntime.getEventSource(oEvent);
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
            var mRequest = AnalyticsDrilldownRuntime.buildBuilderDrilldownRequest(oEvent, sBuilderDimension, sBuilderMetric);
            return queueAnalyticsDrilldown(this, mRequest.filterKey, mRequest.filterValue, mRequest.payload);
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
        onExportAnalyticsReport: function () { return AnalyticsExportRuntime.exportAnalyticsReport(this); },
        onCloseAnalytics: function () { WorkspaceRouteNavigation.navigateBackFromAnalytics(this); },
        formatAnalyticsMatrixMetricLabel: function (sMetricKey) {
            return AnalyticsFormatRuntime.formatMatrixMetricLabel(sMetricKey, getBundleText.bind(null, this));
        },
        formatAnalyticsSourceContext: function (sSelectedSource) {
            return AnalyticsFormatRuntime.formatSourceContext(sSelectedSource, getBundleText.bind(null, this));
        },
        formatRefreshStatusState: function (oRefreshState) { return AnalyticsFormatRuntime.formatRefreshStatusState(oRefreshState); },
        formatRefreshStatusText: function (oRefreshState) { return AnalyticsFormatRuntime.formatRefreshStatusText(oRefreshState, getBundleText.bind(null, this)); },
        formatRefreshEnabled: function (bRefreshBusy, oRefreshState) { return AnalyticsFormatRuntime.formatRefreshEnabled(bRefreshBusy, oRefreshState); },
        formatAnalyticsInfoMessageType: function () { return UiSemanticConstants.MESSAGE_TYPE.INFORMATION; },
        formatAnalyticsWarningMessageType: function () { return UiSemanticConstants.MESSAGE_TYPE.WARNING; },
        formatAnalyticsCompareYearState: function () { return UiSemanticConstants.OBJECT_STATUS_STATE.NONE; },
        formatAnalyticsSelectedYearState: function () { return UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS; },
        formatAnalyticsSourceState: function () { return UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION; },
        formatRateState: function (sRate) { return AnalyticsFormatRuntime.formatRateState(sRate); },
        formatRefreshMessageType: function (oRefreshState) { return AnalyticsFormatRuntime.formatRefreshMessageType(oRefreshState); },
        formatRefreshMessageVisible: function (oRefreshState) { return AnalyticsFormatRuntime.formatRefreshMessageVisible(oRefreshState); }
    });
});
