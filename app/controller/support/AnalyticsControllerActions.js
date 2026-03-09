sap.ui.define([
    "checklist/app/controller/base/ControllerTextRuntime",
    "checklist/app/service/domain/analytics/AnalyticsFacade",
    "checklist/app/controller/support/AnalyticsBuilderSupport",
    "checklist/app/service/framework/NavigationIntentService",
    "checklist/app/service/framework/CtxFactory",
    "checklist/app/service/framework/FacadeCommandRuntime",
    "checklist/app/service/framework/ControllerRouteRuntime",
    "checklist/app/service/framework/ControllerViewStateRuntime",
    "checklist/app/service/framework/SchedulingRuntime"
], function (ControllerTextRuntime, AnalyticsFacade, AnalyticsBuilderSupport, NavigationIntentService, CtxFactory, FacadeCommandRuntime, ControllerRouteRuntime, ControllerViewStateRuntime, SchedulingRuntime) {
    "use strict";

    var getText = ControllerTextRuntime.getText;
    var REFRESH_STATE_TASK_KEY = "ANALYTICS_REFRESH";
    var REFRESH_POLL_DELAY_MS = 1800;
    var REFRESH_POLL_MAX_ATTEMPTS = 12;

    function isRefreshQueued(oRefreshState) {
        var sStatus = String(oRefreshState && oRefreshState.status || "").trim().toUpperCase();
        return !!(oRefreshState && oRefreshState.isRunning) || sStatus === "REQUESTED" || sStatus === "RUNNING";
    }

    function buildCtx(oController) {
        return CtxFactory.buildCtx(oController, {});
    }

    return {
        onInit: function () {
            this._facade = new AnalyticsFacade();
            ControllerViewStateRuntime.initModel(this, function () {
                return AnalyticsBuilderSupport.createInitialViewState(REFRESH_STATE_TASK_KEY);
            });
            AnalyticsBuilderSupport.applyBuilderSelection(this);
            ControllerRouteRuntime.attachMatched(this, [
                { name: "analytics", handler: this._onAnalyticsMatched }
            ]);
        },

        onExit: function () {
            ControllerRouteRuntime.detachAllMatched(this);
            this._facade = null;
        },

        _applyComparisonMetricSelection: function () {
            AnalyticsBuilderSupport.applyComparisonMetricSelection(this);
        },

        _applyBuilderSelection: function (mOverrides) {
            AnalyticsBuilderSupport.applyBuilderSelection(this, mOverrides || {});
        },

        _syncAnalyticsContextHints: function () {
            AnalyticsBuilderSupport.syncAnalyticsContextHints(this);
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
                buildCtx(this)
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
            var oCtx = buildCtx(this);
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
            var oCtx = buildCtx(this);
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
            var sMetric = AnalyticsBuilderSupport.getSelectedKeyFromEvent(oEvent);
            if (!sMetric) {
                return;
            }
            ControllerViewStateRuntime.set(this, "/comparisonMetric", sMetric);
            this._applyComparisonMetricSelection();
        },

        onSelectAnalyticsBuilderDimension: function (oEvent) {
            var sDimension = AnalyticsBuilderSupport.getSelectedKeyFromEvent(oEvent);
            if (!sDimension) {
                return;
            }
            this._applyBuilderSelection({ dimension: sDimension });
        },

        onSelectAnalyticsBuilderMetric: function (oEvent) {
            var sMetric = AnalyticsBuilderSupport.getSelectedKeyFromEvent(oEvent);
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
