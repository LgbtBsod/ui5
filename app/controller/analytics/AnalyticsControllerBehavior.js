sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsBuilderRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRouteRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsYearRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsRefreshRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsDrilldownRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsExportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsLoadRuntime"
], function (ControllerTextRuntime, AnalyticsFacade, AnalyticsBuilderRuntime, NavigationIntentService, CtxFactory, ControllerRouteRuntime, ControllerViewStateRuntime, AnalyticsContracts, ModelContracts, NavigationContracts, AnalyticsYearRuntime, AnalyticsRefreshRuntime, AnalyticsDrilldownRuntime, AnalyticsExportRuntime, AnalyticsLoadRuntime) {
    "use strict";

    var getText = ControllerTextRuntime.getText;
    var REFRESH_STATE_TASK_KEY = AnalyticsContracts.REFRESH.TASK_KEY;
    var SELECTED_YEAR_PATH = "/selectedYear";
    var COMPARE_YEAR_PATH = "/compareYear";

    function extractPresetFromEvent(oEvent) {
        var oSource = oEvent && oEvent.getSource && oEvent.getSource();
        return String((oSource && oSource.data && oSource.data("preset")) || "").trim().toUpperCase();
    }

    function readSelectedSource(oController) {
        return String(ControllerViewStateRuntime.get(oController, "/selectedSource", AnalyticsContracts.SOURCES.ALL) || AnalyticsContracts.SOURCES.ALL).trim().toUpperCase();
    }

    function buildCtx(oController) {
        return CtxFactory.buildCtx(oController, {});
    }

    return {
        onInit: function () {
            this._facade = new AnalyticsFacade();
            ControllerViewStateRuntime.initModel(this, function () {
                return AnalyticsBuilderRuntime.createInitialViewState(REFRESH_STATE_TASK_KEY);
            });
            AnalyticsBuilderRuntime.applyBuilderSelection(this);
            ControllerRouteRuntime.attachMatched(this, [
                { name: NavigationContracts.ROUTES.ANALYTICS, handler: this._onAnalyticsMatched }
            ]);
        },

        onExit: function () {
            ControllerRouteRuntime.detachAllMatched(this);
            if (this._oAnalyticsYearPicker && typeof this._oAnalyticsYearPicker.destroy === "function") {
                this._oAnalyticsYearPicker.destroy();
            }
            if (this._oAnalyticsReportDialog && typeof this._oAnalyticsReportDialog.destroy === "function") {
                this._oAnalyticsReportDialog.destroy();
            }
            this._oAnalyticsYearPicker = null;
            this._pAnalyticsYearPicker = null;
            this._oAnalyticsReportDialog = null;
            this._pAnalyticsReportDialog = null;
            this._facade = null;
        },

        _applyComparisonMetricSelection: function () {
            AnalyticsBuilderRuntime.applyComparisonMetricSelection(this);
        },

        _applyBuilderSelection: function (mOverrides) {
            AnalyticsBuilderRuntime.applyBuilderSelection(this, mOverrides || {});
        },

        _syncAnalyticsContextHints: function () {
            AnalyticsBuilderRuntime.syncAnalyticsContextHints(this);
        },

        _setCompareYearValidation: function (sState, sText) {
            ControllerViewStateRuntime.setMany(this, {
                "/compareYearValueState": sState || "None",
                "/compareYearValueStateText": sText || ""
            });
        },

        _loadAnalytics: function (sReason) {
            return AnalyticsLoadRuntime.loadAnalytics(this, sReason, {
                applyBuilderSelection: function (oController) {
                    AnalyticsBuilderRuntime.applyBuilderSelection(oController);
                },
                applyComparisonMetricSelection: function (oController) {
                    AnalyticsBuilderRuntime.applyComparisonMetricSelection(oController);
                },
                buildCompareYearOptions: function (oController) {
                    return AnalyticsYearRuntime.buildCompareYearOptions(oController, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH);
                },
                buildCtx: buildCtx,
                buildYearOptions: function (oController) {
                    return AnalyticsYearRuntime.buildYearOptions(oController, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH);
                },
                setCompareYearValidation: function (oController, sState, sText) {
                    oController._setCompareYearValidation(sState, sText);
                },
                syncAnalyticsContextHints: function (oController) {
                    AnalyticsBuilderRuntime.syncAnalyticsContextHints(oController);
                },
                syncCompareYearDefaults: function (oController, sSelectedYear) {
                    return AnalyticsYearRuntime.syncCompareYearDefaults(oController, sSelectedYear, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH);
                }
            });
        },

        _pollRefreshStateUntilSettled: function (iAttemptsLeft) {
            var oCtx = buildCtx(this);
            return AnalyticsRefreshRuntime.pollRefreshStateUntilSettled(this, iAttemptsLeft, function () {
                return oCtx && oCtx.analytics && oCtx.analytics.fetchRefreshState ? oCtx.analytics.fetchRefreshState() : null;
            });
        },

        _onAnalyticsMatched: function () {
            return this._loadAnalytics("routeMatched");
        },

        onRefreshAnalytics: function () {
            var oCtx = buildCtx(this);
            var oRefreshState = ControllerViewStateRuntime.get(this, "/refreshState", {}) || {};
            if (AnalyticsRefreshRuntime.isRefreshQueued(oRefreshState)) {
                ControllerViewStateRuntime.set(this, "/refreshBusy", true);
                return this._pollRefreshStateUntilSettled(AnalyticsRefreshRuntime.REFRESH_POLL_MAX_ATTEMPTS).then(function () {
                    return this._loadAnalytics("pollRefresh");
                }.bind(this)).then(function (oResult) {
                    ControllerViewStateRuntime.set(this, "/refreshBusy", false);
                    return oResult;
                }.bind(this), function (oError) {
                    ControllerViewStateRuntime.set(this, "/refreshBusy", false);
                    throw oError;
                }.bind(this));
            }
            ControllerViewStateRuntime.setMany(this, {
                "/refreshBusy": true,
                "/error": ""
            });
            return Promise.resolve(oCtx && oCtx.analytics && oCtx.analytics.requestRefresh ? oCtx.analytics.requestRefresh({
                requestedBy: AnalyticsContracts.REFRESH.REQUESTED_BY_WEB
            }) : null).then(function (oState) {
                if (oState) {
                    ControllerViewStateRuntime.set(this, "/refreshState", oState);
                }
                return this._pollRefreshStateUntilSettled(AnalyticsRefreshRuntime.REFRESH_POLL_MAX_ATTEMPTS);
            }.bind(this)).then(function () {
                return this._loadAnalytics("manualRefresh");
            }.bind(this)).catch(function (oError) {
                ControllerViewStateRuntime.set(this, "/error", String((oError && oError.message) || "Analytics refresh failed"));
                throw oError;
            }.bind(this)).then(function (oResult) {
                ControllerViewStateRuntime.set(this, "/refreshBusy", false);
                return oResult;
            }.bind(this), function (oError) {
                ControllerViewStateRuntime.set(this, "/refreshBusy", false);
                throw oError;
            }.bind(this));
        },

        onSelectAnalyticsYear: function (oEvent) {
            var sYear = AnalyticsYearRuntime.normalizeYearString(
                oEvent && oEvent.getParameter && oEvent.getParameter("selectedItem") && oEvent.getParameter("selectedItem").getKey() ||
                oEvent && oEvent.getParameter && oEvent.getParameter("value") ||
                oEvent && oEvent.getParameter && oEvent.getParameter("selectedKey") ||
                oEvent && oEvent.getSource && oEvent.getSource().getValue && oEvent.getSource().getValue() ||
                oEvent && oEvent.getSource && oEvent.getSource().getSelectedKey && oEvent.getSource().getSelectedKey() ||
                ""
            );
            if (!sYear) {
                if (oEvent && oEvent.getSource && oEvent.getSource().setValue) {
                    oEvent.getSource().setValue(String(ControllerViewStateRuntime.get(this, "/selectedYear", "") || ""));
                }
                return Promise.resolve();
            }
            ControllerViewStateRuntime.set(this, "/selectedYear", sYear);
            ControllerViewStateRuntime.set(this, "/availableYears", AnalyticsYearRuntime.buildYearOptions(this, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH));
            AnalyticsYearRuntime.syncCompareYearDefaults(this, sYear, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH);
            this._setCompareYearValidation("None", "");
            return this._loadAnalytics("yearChanged");
        },

        onLiveChangeAnalyticsYear: function (oEvent) {
            var oInput = oEvent && oEvent.getSource ? oEvent.getSource() : null;
            var sValue = AnalyticsYearRuntime.sanitizeYearValue(oEvent && oEvent.getParameter && oEvent.getParameter("value"));
            if (oInput && oInput.getValue && oInput.getValue() !== sValue && oInput.setValue) {
                oInput.setValue(sValue);
            }
        },

        onSelectAnalyticsSource: function (oEvent) {
            var sSource = String(
                oEvent && oEvent.getParameter && oEvent.getParameter("selectedKey") ||
                oEvent && oEvent.getSource && oEvent.getSource().getSelectedKey && oEvent.getSource().getSelectedKey() ||
                ""
            ).trim().toUpperCase();
            if (!sSource) {
                return Promise.resolve();
            }
            ControllerViewStateRuntime.set(this, "/selectedSource", sSource);
            return this._loadAnalytics("sourceChanged");
        },

        onChangeAnalyticsCompareYear: function (oEvent) {
            var sStoredCompareYear = String(ControllerViewStateRuntime.get(this, "/compareYear", "") || "").trim();
            return AnalyticsYearRuntime.applyCompareYearChange(this, oEvent, sStoredCompareYear, this._setCompareYearValidation.bind(this), this._loadAnalytics.bind(this));
        },

        onLiveChangeAnalyticsCompareYear: function (oEvent) {
            var oInput = oEvent && oEvent.getSource ? oEvent.getSource() : null;
            var sValue = AnalyticsYearRuntime.sanitizeYearValue(oEvent && oEvent.getParameter && oEvent.getParameter("value"));
            if (oInput && oInput.getValue && oInput.getValue() !== sValue && oInput.setValue) {
                oInput.setValue(sValue);
            }
            this._setCompareYearValidation("None", "");
        },

        onOpenAnalyticsSelectedYearPicker: function (oEvent) {
            var oSource = oEvent && oEvent.getSource ? oEvent.getSource() : null;
            AnalyticsYearRuntime.ensureYearPickerRangeForValue(this, "selectedYear");
            return AnalyticsYearRuntime.ensureYearPicker(this).then(function (oPopover) {
                if (oSource) {
                    oPopover.openBy(oSource);
                }
            });
        },

        onOpenAnalyticsCompareYearPicker: function (oEvent) {
            var oSource = oEvent && oEvent.getSource ? oEvent.getSource() : null;
            AnalyticsYearRuntime.ensureYearPickerRangeForValue(this, "compareYear");
            return AnalyticsYearRuntime.ensureYearPicker(this).then(function (oPopover) {
                if (oSource) {
                    oPopover.openBy(oSource);
                }
            });
        },

        onNavigateAnalyticsYearPickerBack: function () {
            var sTargetField = String(ControllerViewStateRuntime.get(this, "/yearPicker/targetField", "selectedYear") || "selectedYear");
            var iRangeStart = Number(ControllerViewStateRuntime.get(this, "/yearPicker/rangeStart", new Date().getFullYear() - 9) || 0);
            AnalyticsYearRuntime.syncYearPickerState(this, sTargetField, iRangeStart - 20);
        },

        onNavigateAnalyticsYearPickerForward: function () {
            var sTargetField = String(ControllerViewStateRuntime.get(this, "/yearPicker/targetField", "selectedYear") || "selectedYear");
            var iRangeStart = Number(ControllerViewStateRuntime.get(this, "/yearPicker/rangeStart", new Date().getFullYear() - 9) || 0);
            AnalyticsYearRuntime.syncYearPickerState(this, sTargetField, iRangeStart + 20);
        },

        onSelectAnalyticsYearFromPicker: function (oEvent) {
            var sTargetField = String(ControllerViewStateRuntime.get(this, "/yearPicker/targetField", "selectedYear") || "selectedYear");
            var oSource = oEvent && oEvent.getSource ? oEvent.getSource() : null;
            var sYear = AnalyticsYearRuntime.normalizeYearString(oSource && oSource.data && oSource.data("year"));
            if (!sYear) {
                return Promise.resolve();
            }
            if (sTargetField === "compareYear") {
                ControllerViewStateRuntime.set(this, "/compareYear", sYear);
                ControllerViewStateRuntime.set(this, "/availableYears", AnalyticsYearRuntime.buildYearOptions(this, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH));
                ControllerViewStateRuntime.set(this, "/compareYearOptions", AnalyticsYearRuntime.buildCompareYearOptions(this, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH));
                this._setCompareYearValidation("None", "");
                AnalyticsYearRuntime.syncYearPickerState(this, sTargetField, Number(ControllerViewStateRuntime.get(this, "/yearPicker/rangeStart", 0) || 0));
                if (this._oAnalyticsYearPicker) {
                    this._oAnalyticsYearPicker.close();
                }
                return this._loadAnalytics("compareYearPicked");
            }
            ControllerViewStateRuntime.set(this, "/selectedYear", sYear);
            ControllerViewStateRuntime.set(this, "/availableYears", AnalyticsYearRuntime.buildYearOptions(this, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH));
            AnalyticsYearRuntime.syncCompareYearDefaults(this, sYear, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH);
            this._setCompareYearValidation("None", "");
            AnalyticsYearRuntime.syncYearPickerState(this, sTargetField, Number(ControllerViewStateRuntime.get(this, "/yearPicker/rangeStart", 0) || 0));
            if (this._oAnalyticsYearPicker) {
                this._oAnalyticsYearPicker.close();
            }
            return this._loadAnalytics("yearPicked");
        },

        onSelectAnalyticsMetric: function (oEvent) {
            var sMetric = AnalyticsBuilderRuntime.getSelectedKeyFromEvent(oEvent);
            if (!sMetric) {
                return;
            }
            ControllerViewStateRuntime.set(this, "/comparisonMetric", sMetric);
            this._applyComparisonMetricSelection();
        },

        onSelectAnalyticsBuilderDimension: function (oEvent) {
            var sDimension = AnalyticsBuilderRuntime.getSelectedKeyFromEvent(oEvent);
            if (!sDimension) {
                return;
            }
            this._applyBuilderSelection({ dimension: sDimension });
        },

        onSelectAnalyticsBuilderMetric: function (oEvent) {
            var sMetric = AnalyticsBuilderRuntime.getSelectedKeyFromEvent(oEvent);
            if (!sMetric) {
                return;
            }
            this._applyBuilderSelection({ metric: sMetric });
        },

        onApplyAnalyticsYearPreset: function (oEvent) {
            var sPreset = extractPresetFromEvent(oEvent);
            if (!sPreset) {
                return Promise.resolve();
            }
            return AnalyticsYearRuntime.applyYearPreset(this, sPreset, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH, this._loadAnalytics.bind(this));
        },

        onDrilldownAnalyticsBuilder: function (oEvent) {
            var sDimension = String(ControllerViewStateRuntime.get(this, "/builderDimension", AnalyticsContracts.BUILDER.FALLBACK_DIMENSION) || AnalyticsContracts.BUILDER.FALLBACK_DIMENSION).trim().toUpperCase();
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            var sLabel = String((oPoint && (oPoint.Dimension || oPoint.label || oPoint.labelShort)) || "").trim();
            var mMap = {
                LPC: "Lpc",
                PROFESSION: "ProfessionText",
                LOCATION: "LocationKey"
            };
            return AnalyticsDrilldownRuntime.queueAnalyticsDrilldown(this, mMap[sDimension], sLabel, {
                dimension: sDimension,
                metric: String(ControllerViewStateRuntime.get(this, "/builderMetric", "") || "").trim().toUpperCase()
            });
        },

        onDrilldownAnalyticsSource: function (oEvent) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            var sSource = String((oPoint && oPoint.Source) || "").trim().toUpperCase();
            if (!sSource || sSource === "ALL") {
                return Promise.resolve(false);
            }
            return AnalyticsDrilldownRuntime.queueAnalyticsDrilldown(this, "Source", sSource, {
                dimension: AnalyticsContracts.DIMENSIONS.SOURCE
            });
        },

        onDrilldownAnalyticsProfession: function (oEvent) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            return AnalyticsDrilldownRuntime.queueAnalyticsDrilldown(this, "ProfessionText", String((oPoint && oPoint.Profession) || "").trim(), {
                dimension: AnalyticsContracts.DIMENSIONS.PROFESSION
            });
        },

        onDrilldownAnalyticsLpc: function (oEvent) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            return AnalyticsDrilldownRuntime.queueAnalyticsDrilldown(this, "Lpc", String((oPoint && oPoint.LPC) || "").trim(), {
                dimension: AnalyticsContracts.DIMENSIONS.LPC
            });
        },

        onDrilldownAnalyticsLocation: function (oEvent) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            return AnalyticsDrilldownRuntime.queueAnalyticsDrilldown(this, "LocationKey", String((oPoint && oPoint.Location) || "").trim(), {
                dimension: AnalyticsContracts.DIMENSIONS.LOCATION
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
        }
    };
});
