sap.ui.define([
    "checklist/app/controller/base/ControllerTextRuntime",
    "checklist/app/service/domain/analytics/AnalyticsFacade",
    "checklist/app/service/domain/analytics/AnalyticsPayloadNormalizer",
    "checklist/app/service/framework/NavigationIntentService",
    "checklist/app/service/framework/ControllerCtxRuntime",
    "checklist/app/service/framework/FacadeCommandRuntime",
    "checklist/app/service/framework/ControllerRouteRuntime",
    "checklist/app/service/framework/ControllerViewStateRuntime",
    "checklist/app/service/framework/SchedulingRuntime"
], function (ControllerTextRuntime, AnalyticsFacade, AnalyticsPayloadNormalizer, NavigationIntentService, ControllerCtxRuntime, FacadeCommandRuntime, ControllerRouteRuntime, ControllerViewStateRuntime, SchedulingRuntime) {
    "use strict";

    var getText = ControllerTextRuntime.getText;
    var REFRESH_STATE_TASK_KEY = "ANALYTICS_REFRESH";
    var REFRESH_POLL_DELAY_MS = 1800;
    var REFRESH_POLL_MAX_ATTEMPTS = 12;
    var BUILDER_DIMENSION_RULES = {
        MONTH: {
            vizType: "column",
            metricKeys: ["TOTAL", "FAILED_CHECKS", "FAILED_BARRIERS", "FAILED_CHECKLISTS", "FAILED_BARRIER_CHECKLISTS"],
            chartKeyByMetric: {
                TOTAL: "monthlyTotal",
                FAILED_CHECKS: "monthlyFailedChecks",
                FAILED_BARRIERS: "monthlyFailedBarriers",
                FAILED_CHECKLISTS: "monthlyFailedChecklists",
                FAILED_BARRIER_CHECKLISTS: "monthlyFailedBarrierChecklists"
            }
        },
        SOURCE: {
            vizType: "bar",
            metricKeys: ["TOTAL", "FAILED_CHECKS", "FAILED_BARRIERS"],
            chartKeyByMetric: {
                TOTAL: "totalBySource",
                FAILED_CHECKS: "failedChecksBySource",
                FAILED_BARRIERS: "failedBarriersBySource"
            }
        },
        PROFESSION: {
            vizType: "bar",
            metricKeys: ["FAILED_CHECKS", "FAILED_BARRIERS"],
            chartKeyByMetric: {
                FAILED_CHECKS: "failedChecksByProfession",
                FAILED_BARRIERS: "failedBarriersByProfession"
            }
        },
        LPC: {
            vizType: "bar",
            metricKeys: ["FAILED_CHECKS", "FAILED_BARRIERS"],
            chartKeyByMetric: {
                FAILED_CHECKS: "failedChecksByLpc",
                FAILED_BARRIERS: "failedBarriersByLpc"
            }
        },
        LOCATION: {
            vizType: "bar",
            metricKeys: ["FAILED_CHECKS", "FAILED_BARRIERS"],
            chartKeyByMetric: {
                FAILED_CHECKS: "failedChecksByLocation",
                FAILED_BARRIERS: "failedBarriersByLocation"
            }
        },
        BUKRS: {
            vizType: "bar",
            metricKeys: ["FAILED_CHECKS", "FAILED_BARRIERS"],
            chartKeyByMetric: {
                FAILED_CHECKS: "failedChecksByBukrs",
                FAILED_BARRIERS: "failedBarriersByBukrs"
            }
        },
        ORGUNIT: {
            vizType: "bar",
            metricKeys: ["FAILED_CHECKS", "FAILED_BARRIERS"],
            chartKeyByMetric: {
                FAILED_CHECKS: "failedChecksByOrgunit",
                FAILED_BARRIERS: "failedBarriersByOrgunit"
            }
        },
        BARRIER_NUMBER: {
            vizType: "bar",
            metricKeys: ["TOTAL", "FAILED_BARRIERS"],
            chartKeyByMetric: {
                TOTAL: "totalBarriersByBarrierNumber",
                FAILED_BARRIERS: "failedBarriersByBarrierNumber"
            }
        }
    };
    var BUILDER_DIMENSION_TEXT_KEY_MAP = {
        MONTH: "analyticsDimensionMonth",
        SOURCE: "analyticsDimensionSource",
        PROFESSION: "analyticsDimensionProfession",
        LPC: "analyticsDimensionLpc",
        LOCATION: "analyticsDimensionLocation",
        BUKRS: "analyticsDimensionBukrs",
        ORGUNIT: "analyticsDimensionOrgunit",
        BARRIER_NUMBER: "analyticsDimensionBarrierNumber"
    };
    var BUILDER_METRIC_TEXT_KEY_MAP = {
        TOTAL: "analyticsMetricTotal",
        FAILED_CHECKS: "analyticsMetricFailedChecks",
        FAILED_BARRIERS: "analyticsMetricFailedBarriers",
        FAILED_CHECKLISTS: "analyticsMetricFailedChecklistCount",
        FAILED_BARRIER_CHECKLISTS: "analyticsMetricFailedBarrierChecklistCount"
    };

    function isRefreshQueued(oRefreshState) {
        var sStatus = String(oRefreshState && oRefreshState.status || "").trim().toUpperCase();
        return !!(oRefreshState && oRefreshState.isRunning) || sStatus === "REQUESTED" || sStatus === "RUNNING";
    }

    function getSelectedKeyFromEvent(oEvent) {
        return String(
            oEvent && oEvent.getParameter && oEvent.getParameter("selectedItem") && oEvent.getParameter("selectedItem").getKey() ||
            oEvent && oEvent.getParameter && oEvent.getParameter("selectedKey") ||
            oEvent && oEvent.getSource && oEvent.getSource().getSelectedKey && oEvent.getSource().getSelectedKey() ||
            ""
        ).trim().toUpperCase();
    }

    function normalizeBuilderDimension(sDimension) {
        var sResolved = String(sDimension || "").trim().toUpperCase();
        return BUILDER_DIMENSION_RULES[sResolved] ? sResolved : "MONTH";
    }

    function buildBuilderDimensionOptions(oController, sSource) {
        var sResolvedSource = String(sSource || "ALL").trim().toUpperCase();
        var aDimensionKeys = ["MONTH", "SOURCE", "PROFESSION", "LPC", "BARRIER_NUMBER"];
        if (sResolvedSource !== "INTEGRATION") {
            aDimensionKeys = aDimensionKeys.concat(["LOCATION", "BUKRS", "ORGUNIT"]);
        }
        return aDimensionKeys.map(function (sDimensionKey) {
            return {
                key: sDimensionKey,
                text: getText(oController, BUILDER_DIMENSION_TEXT_KEY_MAP[sDimensionKey], null, sDimensionKey)
            };
        });
    }

    function normalizeBuilderDimensionForSource(sDimension, sSource) {
        var sResolvedDimension = normalizeBuilderDimension(sDimension);
        var sResolvedSource = String(sSource || "ALL").trim().toUpperCase();
        if (sResolvedSource === "INTEGRATION" && ["LOCATION", "BUKRS", "ORGUNIT"].indexOf(sResolvedDimension) >= 0) {
            return "MONTH";
        }
        return sResolvedDimension;
    }

    function normalizeBuilderMetric(sDimension, sMetric) {
        var sResolvedDimension = normalizeBuilderDimension(sDimension);
        var oRule = BUILDER_DIMENSION_RULES[sResolvedDimension];
        var sResolvedMetric = String(sMetric || "").trim().toUpperCase();
        if (oRule.metricKeys.indexOf(sResolvedMetric) >= 0) {
            return sResolvedMetric;
        }
        return oRule.metricKeys[0];
    }

    function buildBuilderMetricOptions(oController, sDimension) {
        var sResolvedDimension = normalizeBuilderDimension(sDimension);
        return (BUILDER_DIMENSION_RULES[sResolvedDimension].metricKeys || []).map(function (sMetricKey) {
            return {
                key: sMetricKey,
                text: getText(oController, BUILDER_METRIC_TEXT_KEY_MAP[sMetricKey], null, sMetricKey)
            };
        });
    }

    function resolveBuilderChartRows(oAnalytics, sDimension, sMetric) {
        var sResolvedDimension = normalizeBuilderDimension(sDimension);
        var sResolvedMetric = normalizeBuilderMetric(sResolvedDimension, sMetric);
        var oRule = BUILDER_DIMENSION_RULES[sResolvedDimension];
        var sChartKey = oRule && oRule.chartKeyByMetric && oRule.chartKeyByMetric[sResolvedMetric];
        return sChartKey && oAnalytics && oAnalytics.charts && Array.isArray(oAnalytics.charts[sChartKey])
            ? oAnalytics.charts[sChartKey]
            : [];
    }

    function buildInitialViewState() {
        var iCurrentYear = new Date().getFullYear();
        return {
            busy: false,
            error: "",
            refreshBusy: false,
            selectedYear: String(iCurrentYear),
            compareYear: String(iCurrentYear - 1),
            compareYearValueState: "None",
            compareYearValueStateText: "",
            comparisonMetric: "FAILED_CHECKLISTS",
            builderDimension: "MONTH",
            builderMetric: "FAILED_CHECKLISTS",
            builderDimensionOptions: [],
            builderMetricOptions: [],
            builderChartRows: [],
            builderChartTitle: "",
            builderVizType: "column",
            builderChartHasData: false,
            builderSourceHintText: "",
            compareYearHasData: true,
            compareYearHintText: "",
            availableYears: [{ key: String(iCurrentYear), text: String(iCurrentYear) }],
            selectedSource: "ALL",
            refreshState: {
                taskKey: REFRESH_STATE_TASK_KEY,
                taskName: "Analytics Refresh",
                status: "IDLE",
                isRunning: false,
                requestedAt: "",
                requestedBy: "",
                startedAt: "",
                finishedAt: "",
                lastSuccessAt: "",
                lastError: "",
                lastMessage: "",
                activeRunId: ""
            },
            analytics: AnalyticsPayloadNormalizer.createEmptyDashboard()
        };
    }

    return {
        onInit: function () {
            this._facade = new AnalyticsFacade();
            ControllerViewStateRuntime.initModel(this, buildInitialViewState);
            this._applyBuilderSelection();
            ControllerRouteRuntime.attachMatched(this, [
                { name: "analytics", handler: this._onAnalyticsMatched }
            ]);
        },

        onExit: function () {
            ControllerRouteRuntime.detachAllMatched(this);
            this._facade = null;
        },

        _applyComparisonMetricSelection: function () {
            var sMetric = String(ControllerViewStateRuntime.get(this, "/comparisonMetric", "FAILED_CHECKLISTS") || "FAILED_CHECKLISTS").trim().toUpperCase();
            var oAnalytics = ControllerViewStateRuntime.get(this, "/analytics", {}) || {};
            var mSeries = oAnalytics.comparisonMetricSeries || {};
            var aRows = Array.isArray(mSeries[sMetric]) ? mSeries[sMetric] : [];
            ControllerViewStateRuntime.set(this, "/analytics/comparisonChartRows", aRows);
            ControllerViewStateRuntime.set(this, "/comparisonMetric", sMetric);
        },

        _applyBuilderSelection: function (mOverrides) {
            var oAnalytics = ControllerViewStateRuntime.get(this, "/analytics", {}) || {};
            var sSelectedSource = String(ControllerViewStateRuntime.get(this, "/selectedSource", "ALL") || "ALL").trim().toUpperCase();
            var sDimension = normalizeBuilderDimensionForSource(
                (mOverrides && mOverrides.dimension) || ControllerViewStateRuntime.get(this, "/builderDimension", "MONTH"),
                sSelectedSource
            );
            var sMetric = normalizeBuilderMetric(sDimension, (mOverrides && mOverrides.metric) || ControllerViewStateRuntime.get(this, "/builderMetric", "FAILED_CHECKLISTS"));
            var aMetricOptions = buildBuilderMetricOptions(this, sDimension);
            var aDimensionOptions = buildBuilderDimensionOptions(this, sSelectedSource);
            var aRows = resolveBuilderChartRows(oAnalytics, sDimension, sMetric);
            var sMetricText = getText(this, BUILDER_METRIC_TEXT_KEY_MAP[sMetric], null, sMetric);
            var sDimensionText = getText(this, BUILDER_DIMENSION_TEXT_KEY_MAP[sDimension], null, sDimension);

            ControllerViewStateRuntime.setMany(this, {
                "/builderDimension": sDimension,
                "/builderMetric": sMetric,
                "/builderDimensionOptions": aDimensionOptions,
                "/builderMetricOptions": aMetricOptions,
                "/builderChartRows": aRows,
                "/builderChartTitle": getText(this, "analyticsBuilderTitlePattern", [sMetricText, sDimensionText], sMetricText + " by " + sDimensionText),
                "/builderVizType": BUILDER_DIMENSION_RULES[sDimension].vizType,
                "/builderChartHasData": Array.isArray(aRows) && aRows.length > 0
            });
        },

        _syncAnalyticsContextHints: function () {
            var oAnalytics = ControllerViewStateRuntime.get(this, "/analytics", {}) || {};
            var sSelectedSource = String(ControllerViewStateRuntime.get(this, "/selectedSource", "ALL") || "ALL").trim().toUpperCase();
            var sBuilderDimension = String(ControllerViewStateRuntime.get(this, "/builderDimension", "MONTH") || "MONTH").trim().toUpperCase();
            var iSelectedYear = Number(oAnalytics.selectedYear || 0);
            var iCompareYear = Number(ControllerViewStateRuntime.get(this, "/compareYear", 0) || 0);
            var bCompareYearHasData = iSelectedYear === iCompareYear || !!oAnalytics.compareYearHasData;
            var sCompareYearHintText = bCompareYearHasData
                ? ""
                : getText(this, "analyticsCompareYearNoData", [String(iCompareYear || "")], "No aggregated data for compare year " + String(iCompareYear || ""));
            var sBuilderSourceHintText = "";
            if (sSelectedSource === "INTEGRATION") {
                sBuilderSourceHintText = getText(
                    this,
                    "analyticsIntegrationDimensionsNote",
                    [],
                    "Integration data can be analysed by month, LPC, profession, source and barrier number until enrichment fills BUKRS, location and observer org unit."
                );
            } else if (sSelectedSource === "ALL" && ["LOCATION", "BUKRS", "ORGUNIT"].indexOf(sBuilderDimension) >= 0) {
                sBuilderSourceHintText = getText(
                    this,
                    "analyticsWebEnrichedDimensionsNote",
                    [],
                    "Web-enriched dimensions exclude incomplete integration records until enrichment fills BUKRS, location and observer org unit."
                );
            }
            ControllerViewStateRuntime.setMany(this, {
                "/compareYearHasData": bCompareYearHasData,
                "/compareYearHintText": sCompareYearHintText,
                "/builderSourceHintText": sBuilderSourceHintText
            });
        },

        _setCompareYearValidation: function (sState, sText) {
            ControllerViewStateRuntime.setMany(this, {
                "/compareYearValueState": sState || "None",
                "/compareYearValueStateText": sText || ""
            });
        },

        _loadAnalytics: function (sReason) {
            var sSelectedYear = String(ControllerViewStateRuntime.get(this, "/selectedYear", "") || "").trim();
            var sCompareYear = String(ControllerViewStateRuntime.get(this, "/compareYear", "") || "").trim();
            var sSelectedSource = String(ControllerViewStateRuntime.get(this, "/selectedSource", "ALL") || "ALL").trim().toUpperCase();
            ControllerViewStateRuntime.setMany(this, {
                "/busy": true,
                "/error": ""
            });
            return FacadeCommandRuntime.executeRaw(
                this,
                this._facade,
                "load",
                {
                    reason: sReason || "manual",
                    selectedYear: Number(sSelectedYear) || 0,
                    compareYear: Number(sCompareYear) || 0,
                    selectedSource: sSelectedSource
                },
                ControllerCtxRuntime.buildDefault(this)
            ).then(function (oResult) {
                var oAnalytics = ControllerViewStateRuntime.get(this, "/analytics", {}) || {};
                if (Array.isArray(oAnalytics.availableYears) && oAnalytics.availableYears.length) {
                    ControllerViewStateRuntime.set(this, "/availableYears", oAnalytics.availableYears);
                }
                if (oAnalytics.selectedYear) {
                    ControllerViewStateRuntime.set(this, "/selectedYear", String(oAnalytics.selectedYear));
                }
                if (oAnalytics.compareYear) {
                    ControllerViewStateRuntime.set(this, "/compareYear", String(oAnalytics.compareYear));
                }
                if (oAnalytics.source) {
                    ControllerViewStateRuntime.set(this, "/selectedSource", String(oAnalytics.source));
                }
                if (oAnalytics.refreshState) {
                    ControllerViewStateRuntime.set(this, "/refreshState", oAnalytics.refreshState);
                }
                this._setCompareYearValidation("None", "");
                this._applyComparisonMetricSelection();
                this._applyBuilderSelection();
                this._syncAnalyticsContextHints();
                return oResult;
            }.bind(this));
        },

        _pollRefreshStateUntilSettled: function (iAttemptsLeft) {
            var oCtx = ControllerCtxRuntime.buildDefault(this);
            var iRemaining = Number(iAttemptsLeft);
            return Promise.resolve(oCtx && oCtx.analytics && oCtx.analytics.fetchRefreshState ? oCtx.analytics.fetchRefreshState() : null).then(function (oState) {
                var oRefreshState = oState || {};
                var sStatus = String(oRefreshState.status || "").toUpperCase();
                var bActive = !!oRefreshState.isRunning || sStatus === "REQUESTED" || sStatus === "RUNNING";
                ControllerViewStateRuntime.set(this, "/refreshState", oRefreshState);
                if (!bActive || iRemaining <= 0) {
                    return oRefreshState;
                }
                return SchedulingRuntime.wait(REFRESH_POLL_DELAY_MS).then(function () {
                    return this._pollRefreshStateUntilSettled(iRemaining - 1);
                }.bind(this));
            }.bind(this));
        },

        _onAnalyticsMatched: function () {
            return this._loadAnalytics("routeMatched");
        },

        onRefreshAnalytics: function () {
            var oCtx = ControllerCtxRuntime.buildDefault(this);
            var oRefreshState = ControllerViewStateRuntime.get(this, "/refreshState", {}) || {};
            if (isRefreshQueued(oRefreshState)) {
                ControllerViewStateRuntime.set(this, "/refreshBusy", true);
                return this._pollRefreshStateUntilSettled(REFRESH_POLL_MAX_ATTEMPTS).then(function () {
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
                requestedBy: "WEB"
            }) : null).then(function (oState) {
                if (oState) {
                    ControllerViewStateRuntime.set(this, "/refreshState", oState);
                }
                return this._pollRefreshStateUntilSettled(REFRESH_POLL_MAX_ATTEMPTS);
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
            var sYear = String(
                oEvent && oEvent.getParameter && oEvent.getParameter("selectedItem") && oEvent.getParameter("selectedItem").getKey() ||
                oEvent && oEvent.getParameter && oEvent.getParameter("selectedKey") ||
                oEvent && oEvent.getSource && oEvent.getSource().getSelectedKey && oEvent.getSource().getSelectedKey() ||
                ""
            ).trim();
            if (!sYear) {
                return Promise.resolve();
            }
            ControllerViewStateRuntime.set(this, "/selectedYear", sYear);
            ControllerViewStateRuntime.set(this, "/compareYear", String(Math.max((Number(sYear) || 0) - 1, 0)));
            this._setCompareYearValidation("None", "");
            return this._loadAnalytics("yearChanged");
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
            var oInput = oEvent && oEvent.getSource ? oEvent.getSource() : null;
            var sRawYear = String(
                oEvent && oEvent.getParameter && oEvent.getParameter("value") ||
                oInput && oInput.getValue && oInput.getValue() ||
                ""
            ).trim();
            var sStoredCompareYear = String(ControllerViewStateRuntime.get(this, "/compareYear", "") || "").trim();
            if (!/^\d{4}$/.test(sRawYear)) {
                if (oInput && oInput.setValue) {
                    oInput.setValue(sStoredCompareYear);
                }
                this._setCompareYearValidation("Error", getText(this, "analyticsCompareYearInvalid", [], "Enter a valid four-digit year"));
                return Promise.resolve();
            }
            var iYear = Number(sRawYear);
            if (!Number.isFinite(iYear) || iYear <= 0) {
                if (oInput && oInput.setValue) {
                    oInput.setValue(sStoredCompareYear);
                }
                this._setCompareYearValidation("Error", getText(this, "analyticsCompareYearInvalid", [], "Enter a valid four-digit year"));
                return Promise.resolve();
            }
            ControllerViewStateRuntime.set(this, "/compareYear", String(iYear));
            this._setCompareYearValidation("None", "");
            return this._loadAnalytics("compareYearChanged");
        },

        onSelectAnalyticsMetric: function (oEvent) {
            var sMetric = getSelectedKeyFromEvent(oEvent);
            if (!sMetric) {
                return;
            }
            ControllerViewStateRuntime.set(this, "/comparisonMetric", sMetric);
            this._applyComparisonMetricSelection();
        },

        onSelectAnalyticsBuilderDimension: function (oEvent) {
            var sDimension = getSelectedKeyFromEvent(oEvent);
            if (!sDimension) {
                return;
            }
            this._applyBuilderSelection({ dimension: sDimension });
        },

        onSelectAnalyticsBuilderMetric: function (oEvent) {
            var sMetric = getSelectedKeyFromEvent(oEvent);
            if (!sMetric) {
                return;
            }
            this._applyBuilderSelection({ metric: sMetric });
        },

        onCloseAnalytics: function () {
            NavigationIntentService.navigateBackFromAnalytics(this);
        }
    };
});
