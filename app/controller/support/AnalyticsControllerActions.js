sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsFacade",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/AnalyticsBuilderRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FacadeCommandRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRouteRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "sap/m/ResponsivePopover",
    "sap/m/List",
    "sap/m/StandardListItem"
], function (ControllerTextRuntime, AnalyticsFacade, AnalyticsBuilderRuntime, NavigationIntentService, CtxFactory, FacadeCommandRuntime, ControllerRouteRuntime, ControllerViewStateRuntime, SchedulingRuntime, ModelStateRuntime, ResponsivePopover, List, StandardListItem) {
    "use strict";

    var getText = ControllerTextRuntime.getText;
    var REFRESH_STATE_TASK_KEY = "ANALYTICS_REFRESH";
    var REFRESH_POLL_DELAY_MS = 1800;
    var REFRESH_POLL_MAX_ATTEMPTS = 12;

    function buildCompareYearOptions(oController) {
        var mSeen = {};
        var aOptions = [];

        function pushYear(vYear) {
            var sYear = String(vYear || "").trim();
            if (!/^\d{4}$/.test(sYear) || mSeen[sYear]) {
                return;
            }
            mSeen[sYear] = true;
            aOptions.push({
                key: sYear,
                text: sYear
            });
        }

        (ControllerViewStateRuntime.get(oController, "/availableYears", []) || []).forEach(function (oYear) {
            pushYear((oYear && (oYear.key || oYear.text)) || "");
        });
        pushYear(ControllerViewStateRuntime.get(oController, "/selectedYear", ""));
        pushYear(ControllerViewStateRuntime.get(oController, "/compareYear", ""));
        pushYear(Number(ControllerViewStateRuntime.get(oController, "/selectedYear", 0)) - 1);

        return aOptions.sort(function (aLeft, aRight) {
            return Number(aRight && aRight.key) - Number(aLeft && aLeft.key);
        });
    }

    function syncCompareYearDefaults(oController, sSelectedYear) {
        var iSelectedYear = Number(String(sSelectedYear || "").trim());
        var sDefaultCompareYear = iSelectedYear > 0 ? String(iSelectedYear - 1) : "";
        ControllerViewStateRuntime.set(oController, "/compareYear", sDefaultCompareYear);
        ControllerViewStateRuntime.set(oController, "/compareYearOptions", buildCompareYearOptions(oController));
        return sDefaultCompareYear;
    }

    function extractPresetFromEvent(oEvent) {
        var oSource = oEvent && oEvent.getSource && oEvent.getSource();
        return String((oSource && oSource.data && oSource.data("preset")) || "").trim().toUpperCase();
    }

    function applyYearPreset(oController, sPreset) {
        var iCurrentYear = new Date().getFullYear();
        var iSelectedYear = sPreset === "PREVIOUS" ? (iCurrentYear - 1) : iCurrentYear;
        var sSelectedYear = String(iSelectedYear);
        syncCompareYearDefaults(oController, sSelectedYear);
        ControllerViewStateRuntime.setMany(oController, {
            "/selectedYear": sSelectedYear,
            "/activeYearPreset": sPreset || "CURRENT"
        });
        return oController._loadAnalytics("yearPresetChanged");
    }

    function readSelectedSource(oController) {
        return String(ControllerViewStateRuntime.get(oController, "/selectedSource", "ALL") || "ALL").trim().toUpperCase();
    }

    function buildSearchDrilldownIntent(sFilterKey, sFilterValue, oController, mExtras) {
        return {
            source: "analytics",
            filterKey: String(sFilterKey || "").trim(),
            filterValue: String(sFilterValue || "").trim(),
            selectedYear: String(ControllerViewStateRuntime.get(oController, "/selectedYear", "") || "").trim(),
            compareYear: String(ControllerViewStateRuntime.get(oController, "/compareYear", "") || "").trim(),
            analyticsSource: readSelectedSource(oController),
            extras: Object.assign({}, mExtras || {})
        };
    }

    function extractDrilldownPayload(oEvent) {
        var aData = oEvent && oEvent.getParameter && oEvent.getParameter("data");
        var oEntry = Array.isArray(aData) && aData.length ? aData[0] : null;
        var oPoint = oEntry && (oEntry.data || oEntry.dataContext || {});
        return oPoint || {};
    }

    function queueAnalyticsDrilldown(oController, sFilterKey, sFilterValue, mExtras) {
        var sValue = String(sFilterValue || "").trim();
        if (!sFilterKey || !sValue) {
            return Promise.resolve(false);
        }
        ModelStateRuntime.write(oController, "state", "/analyticsDrilldownIntent", buildSearchDrilldownIntent(sFilterKey, sValue, oController, mExtras));
        NavigationIntentService.navigateToSearch(oController);
        return Promise.resolve(true);
    }

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
                return AnalyticsBuilderRuntime.createInitialViewState(REFRESH_STATE_TASK_KEY);
            });
            AnalyticsBuilderRuntime.applyBuilderSelection(this);
            ControllerRouteRuntime.attachMatched(this, [
                { name: "analytics", handler: this._onAnalyticsMatched }
            ]);
        },

        onExit: function () {
            ControllerRouteRuntime.detachAllMatched(this);
            if (this._oCompareYearPopover && typeof this._oCompareYearPopover.destroy === "function") {
                this._oCompareYearPopover.destroy();
            }
            this._oCompareYearPopover = null;
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
                } else if (oAnalytics.selectedYear) {
                    syncCompareYearDefaults(this, String(oAnalytics.selectedYear));
                }
                if (oAnalytics.source) {
                    ControllerViewStateRuntime.set(this, "/selectedSource", String(oAnalytics.source));
                }
                if (oAnalytics.refreshState) {
                    ControllerViewStateRuntime.set(this, "/refreshState", oAnalytics.refreshState);
                }
                ControllerViewStateRuntime.set(this, "/compareYearOptions", buildCompareYearOptions(this));
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
            syncCompareYearDefaults(this, sYear);
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
            ControllerViewStateRuntime.set(this, "/compareYearOptions", buildCompareYearOptions(this));
            this._setCompareYearValidation("None", "");
            return this._loadAnalytics("compareYearChanged");
        },

        onOpenAnalyticsCompareYearHelp: function (oEvent) {
            var oSource = oEvent && oEvent.getSource ? oEvent.getSource() : null;
            var aOptions = buildCompareYearOptions(this);
            var oList;

            if (!oSource) {
                return;
            }
            ControllerViewStateRuntime.set(this, "/compareYearOptions", aOptions);
            if (!this._oCompareYearPopover) {
                oList = new List({
                    mode: "SingleSelectMaster",
                    growing: false,
                    includeItemInSelection: true,
                    selectionChange: function (oSelectEvent) {
                        var oItem = oSelectEvent.getParameter("listItem");
                        var sYear = String(oItem && oItem.getTitle && oItem.getTitle() || "").trim();
                        if (!/^\d{4}$/.test(sYear)) {
                            return;
                        }
                        ControllerViewStateRuntime.set(this, "/compareYear", sYear);
                        ControllerViewStateRuntime.set(this, "/compareYearOptions", buildCompareYearOptions(this));
                        this._setCompareYearValidation("None", "");
                        this._oCompareYearPopover.close();
                        this._loadAnalytics("compareYearQuickPick");
                    }.bind(this),
                    items: {
                        path: "view>/compareYearOptions",
                        templateShareable: true,
                        template: new StandardListItem({
                            title: "{view>text}",
                            type: "Active"
                        })
                    }
                });
                this._oCompareYearPopover = new ResponsivePopover({
                    placement: "Bottom",
                    contentWidth: "12rem",
                    contentHeight: "16rem",
                    content: [oList]
                });
                this.getView().addDependent(this._oCompareYearPopover);
            }
            this._oCompareYearPopover.openBy(oSource);
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
            return applyYearPreset(this, sPreset);
        },

        onDrilldownAnalyticsBuilder: function (oEvent) {
            var sDimension = String(ControllerViewStateRuntime.get(this, "/builderDimension", "MONTH") || "MONTH").trim().toUpperCase();
            var oPoint = extractDrilldownPayload(oEvent);
            var sLabel = String((oPoint && (oPoint.Dimension || oPoint.label || oPoint.labelShort)) || "").trim();
            var mMap = {
                LPC: "Lpc",
                PROFESSION: "ProfessionText",
                LOCATION: "LocationKey"
            };
            return queueAnalyticsDrilldown(this, mMap[sDimension], sLabel, {
                dimension: sDimension,
                metric: String(ControllerViewStateRuntime.get(this, "/builderMetric", "") || "").trim().toUpperCase()
            });
        },

        onDrilldownAnalyticsSource: function (oEvent) {
            var oPoint = extractDrilldownPayload(oEvent);
            var sSource = String((oPoint && oPoint.Source) || "").trim().toUpperCase();
            if (!sSource || sSource === "ALL") {
                return Promise.resolve(false);
            }
            return queueAnalyticsDrilldown(this, "Source", sSource, {
                dimension: "SOURCE"
            });
        },

        onDrilldownAnalyticsProfession: function (oEvent) {
            var oPoint = extractDrilldownPayload(oEvent);
            return queueAnalyticsDrilldown(this, "ProfessionText", String((oPoint && oPoint.Profession) || "").trim(), {
                dimension: "PROFESSION"
            });
        },

        onDrilldownAnalyticsLpc: function (oEvent) {
            var oPoint = extractDrilldownPayload(oEvent);
            return queueAnalyticsDrilldown(this, "Lpc", String((oPoint && oPoint.LPC) || "").trim(), {
                dimension: "LPC"
            });
        },

        onDrilldownAnalyticsLocation: function (oEvent) {
            var oPoint = extractDrilldownPayload(oEvent);
            return queueAnalyticsDrilldown(this, "LocationKey", String((oPoint && oPoint.Location) || "").trim(), {
                dimension: "LOCATION"
            });
        },

        onCloseAnalytics: function () {
            NavigationIntentService.navigateBackFromAnalytics(this);
        }
    };
});
