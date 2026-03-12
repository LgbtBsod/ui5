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
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "sap/ui/core/Fragment",
    "PRODUCTION_CONTROL_CHECKLIST/util/ExcelExport",
    "PRODUCTION_CONTROL_CHECKLIST/util/analytics/AnalyticsExportRows",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/DialogContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/NavigationContracts"
], function (ControllerTextRuntime, AnalyticsFacade, AnalyticsBuilderRuntime, NavigationIntentService, CtxFactory, FacadeCommandRuntime, ControllerRouteRuntime, ControllerViewStateRuntime, SchedulingRuntime, ModelStateRuntime, StatePaths, Fragment, ExcelExport, AnalyticsExportRows, FeedbackCoordinator, AnalyticsContracts, ModelContracts, DialogContracts, NavigationContracts) {
    "use strict";

    var getText = ControllerTextRuntime.getText;
    var REFRESH_STATE_TASK_KEY = AnalyticsContracts.REFRESH.TASK_KEY;
    var REFRESH_POLL_DELAY_MS = AnalyticsContracts.REFRESH.POLL_DELAY_MS;
    var REFRESH_POLL_MAX_ATTEMPTS = AnalyticsContracts.REFRESH.POLL_MAX_ATTEMPTS;
    var MODELS = ModelContracts.MODELS;
    var TOKENS = ModelContracts.TOKENS;
    var STATE_MODEL = MODELS.STATE;
    var ANALYTICS_DRILLDOWN_INTENT_PATH = "/analyticsDrilldownIntent";
    var SELECTED_YEAR_PATH = "/selectedYear";
    var COMPARE_YEAR_PATH = "/compareYear";

    function isValidYearString(sYear) {
        return /^\d{4}$/.test(String(sYear || "").trim());
    }

    function normalizeYearString(vYear) {
        var sYear = String(vYear || "").trim();
        var iYear;
        if (!isValidYearString(sYear)) {
            return "";
        }
        iYear = Number(sYear);
        return Number.isFinite(iYear) && iYear > 0 ? String(iYear) : "";
    }

    function buildYearOptions(oController) {
        var mSeen = {};
        var aOptions = [];
        var iCurrentYear = new Date().getFullYear();
        var iStartYear = 1950;
        var iEndYear = iCurrentYear + 5;

        function pushYear(vYear) {
            var sYear = normalizeYearString(vYear);
            if (!sYear || mSeen[sYear]) {
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
        pushYear(ControllerViewStateRuntime.get(oController, SELECTED_YEAR_PATH, ""));
        pushYear(ControllerViewStateRuntime.get(oController, COMPARE_YEAR_PATH, ""));
        for (; iEndYear >= iStartYear; iEndYear -= 1) {
            pushYear(iEndYear);
        }

        return aOptions.sort(function (aLeft, aRight) {
            return Number(aRight && aRight.key) - Number(aLeft && aLeft.key);
        });
    }

    function buildCompareYearOptions(oController) {
        var aOptions = buildYearOptions(oController);
        var sDefaultCompareYear = normalizeYearString(Number(ControllerViewStateRuntime.get(oController, SELECTED_YEAR_PATH, 0)) - 1);
        if (sDefaultCompareYear && !aOptions.some(function (oYear) { return oYear && oYear.key === sDefaultCompareYear; })) {
            aOptions.push({
                key: sDefaultCompareYear,
                text: sDefaultCompareYear
            });
        }
        return aOptions.sort(function (aLeft, aRight) {
            return Number(aRight && aRight.key) - Number(aLeft && aLeft.key);
        });
    }

    function sanitizeYearValue(vValue) {
        return String(vValue || "").replace(/\D+/g, "").slice(0, 4);
    }

    function buildYearPickerItems(iRangeStart, sTargetField, oController) {
        var aItems = [];
        var iYear;
        var sActiveYear = String(ControllerViewStateRuntime.get(oController, "/" + sTargetField, "") || "").trim();
        for (iYear = iRangeStart; iYear < iRangeStart + 20; iYear += 1) {
            aItems.push({
                key: String(iYear),
                text: String(iYear),
                selected: String(iYear) === sActiveYear
            });
        }
        return aItems;
    }

    function syncYearPickerState(oController, sTargetField, iRangeStart) {
        var iStart = Number(iRangeStart);
        var iSafeStart = Number.isFinite(iStart) ? iStart : (new Date().getFullYear() - 9);
        ControllerViewStateRuntime.setMany(oController, {
            "/yearPicker/targetField": sTargetField,
            "/yearPicker/rangeStart": iSafeStart,
            "/yearPicker/rangeEnd": iSafeStart + 19,
            "/yearPicker/rangeLabel": String(iSafeStart) + " - " + String(iSafeStart + 19),
            "/yearPicker/items": buildYearPickerItems(iSafeStart, sTargetField, oController)
        });
    }

    function ensureYearPickerRangeForValue(oController, sTargetField) {
        var sYear = normalizeYearString(ControllerViewStateRuntime.get(oController, "/" + sTargetField, ""));
        var iYear = Number(sYear);
        var iRangeStart;
        if (!iYear) {
            syncYearPickerState(oController, sTargetField, new Date().getFullYear() - 9);
            return;
        }
        iRangeStart = iYear - ((iYear - 1) % 20);
        syncYearPickerState(oController, sTargetField, iRangeStart);
    }

    function ensureYearPicker(oController) {
        if (oController._pAnalyticsYearPicker) {
            return oController._pAnalyticsYearPicker;
        }
        oController._pAnalyticsYearPicker = Fragment.load({
            id: oController.getView().getId(),
            name: DialogContracts.getFragmentName(DialogContracts.IDS.ANALYTICS_YEAR_PICKER),
            controller: oController
        }).then(function (oPopover) {
            oController.getView().addDependent(oPopover);
            oController._oAnalyticsYearPicker = oPopover;
            return oPopover;
        });
        return oController._pAnalyticsYearPicker;
    }

    function syncCompareYearDefaults(oController, sSelectedYear) {
        var iSelectedYear = Number(String(sSelectedYear || "").trim());
        var sDefaultCompareYear = iSelectedYear > 0 ? String(iSelectedYear - 1) : "";
        ControllerViewStateRuntime.set(oController, "/compareYear", sDefaultCompareYear);
        ControllerViewStateRuntime.set(oController, "/availableYears", buildYearOptions(oController));
        ControllerViewStateRuntime.set(oController, "/compareYearOptions", buildCompareYearOptions(oController));
        return sDefaultCompareYear;
    }

    function extractPresetFromEvent(oEvent) {
        var oSource = oEvent && oEvent.getSource && oEvent.getSource();
        return String((oSource && oSource.data && oSource.data("preset")) || "").trim().toUpperCase();
    }

    function applyYearPreset(oController, sPreset) {
        var iCurrentYear = new Date().getFullYear();
        var iSelectedYear = sPreset === AnalyticsContracts.YEAR_PRESETS.PREVIOUS ? (iCurrentYear - 1) : iCurrentYear;
        var sSelectedYear = String(iSelectedYear);
        syncCompareYearDefaults(oController, sSelectedYear);
        ControllerViewStateRuntime.setMany(oController, {
            "/selectedYear": sSelectedYear,
            "/activeYearPreset": sPreset || AnalyticsContracts.YEAR_PRESETS.CURRENT
        });
        return oController._loadAnalytics("yearPresetChanged");
    }

    function readSelectedSource(oController) {
        return String(ControllerViewStateRuntime.get(oController, "/selectedSource", AnalyticsContracts.SOURCES.ALL) || AnalyticsContracts.SOURCES.ALL).trim().toUpperCase();
    }

    function buildSearchDrilldownIntent(sFilterKey, sFilterValue, oController, mExtras) {
        return {
            source: TOKENS.ANALYTICS,
            filterKey: String(sFilterKey || "").trim(),
            filterValue: String(sFilterValue || "").trim(),
            selectedYear: String(ControllerViewStateRuntime.get(oController, SELECTED_YEAR_PATH, "") || "").trim(),
            compareYear: String(ControllerViewStateRuntime.get(oController, COMPARE_YEAR_PATH, "") || "").trim(),
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
        ModelStateRuntime.write(oController, STATE_MODEL, ANALYTICS_DRILLDOWN_INTENT_PATH, buildSearchDrilldownIntent(sFilterKey, sValue, oController, mExtras));
        NavigationIntentService.navigateToSearch(oController);
        return Promise.resolve(true);
    }

    function isRefreshQueued(oRefreshState) {
        var sStatus = String(oRefreshState && oRefreshState.status || "").trim().toUpperCase();
        return !!(oRefreshState && oRefreshState.isRunning) ||
            sStatus === AnalyticsContracts.REFRESH.STATUSES.REQUESTED ||
            sStatus === AnalyticsContracts.REFRESH.STATUSES.RUNNING;
    }

    function buildCtx(oController) {
        return CtxFactory.buildCtx(oController, {});
    }

    function ensureAnalyticsReportDialog(oController) {
        if (oController._pAnalyticsReportDialog) {
            return oController._pAnalyticsReportDialog;
        }
        oController._pAnalyticsReportDialog = Fragment.load({
            id: oController.getView().getId(),
            name: DialogContracts.getFragmentName(DialogContracts.IDS.ANALYTICS_REPORT),
            controller: oController
        }).then(function (oDialog) {
            oController.getView().addDependent(oDialog);
            oController._oAnalyticsReportDialog = oDialog;
            return oDialog;
        });
        return oController._pAnalyticsReportDialog;
    }

    function buildAnalyticsExportFileName(oController) {
        var sSource = readSelectedSource(oController).toLowerCase();
        var sYear = String(ControllerViewStateRuntime.get(oController, "/selectedYear", "") || "").trim();
        return ["analytics", sSource || "all", sYear || "scope"].join("_");
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
            var sSelectedYear = String(ControllerViewStateRuntime.get(this, "/selectedYear", "") || "").trim();
            var sCompareYear = String(ControllerViewStateRuntime.get(this, "/compareYear", "") || "").trim();
            var sSelectedSource = String(ControllerViewStateRuntime.get(this, "/selectedSource", AnalyticsContracts.SOURCES.ALL) || AnalyticsContracts.SOURCES.ALL).trim().toUpperCase();
            ModelStateRuntime.write(this, "state", StatePaths.UI_BUSY_ANALYTICS, true);
            ModelStateRuntime.write(this, "state", StatePaths.READINESS_ANALYTICS, {
                status: "loading",
                ready: false,
                readyAt: "",
                error: ""
            });
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
                ControllerViewStateRuntime.set(this, "/availableYears", buildYearOptions(this));
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
                var bActive = !!oRefreshState.isRunning ||
                    sStatus === AnalyticsContracts.REFRESH.STATUSES.REQUESTED ||
                    sStatus === AnalyticsContracts.REFRESH.STATUSES.RUNNING;
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
                requestedBy: AnalyticsContracts.REFRESH.REQUESTED_BY_WEB
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
            var sYear = normalizeYearString(
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
            ControllerViewStateRuntime.set(this, "/availableYears", buildYearOptions(this));
            syncCompareYearDefaults(this, sYear);
            this._setCompareYearValidation("None", "");
            return this._loadAnalytics("yearChanged");
        },

        onLiveChangeAnalyticsYear: function (oEvent) {
            var oInput = oEvent && oEvent.getSource ? oEvent.getSource() : null;
            var sValue = sanitizeYearValue(oEvent && oEvent.getParameter && oEvent.getParameter("value"));
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
            var oInput = oEvent && oEvent.getSource ? oEvent.getSource() : null;
            var sRawYear = sanitizeYearValue(
                oEvent && oEvent.getParameter && oEvent.getParameter("value") ||
                oInput && oInput.getValue && oInput.getValue() ||
                ""
            );
            var sStoredCompareYear = String(ControllerViewStateRuntime.get(this, "/compareYear", "") || "").trim();
            if (!isValidYearString(sRawYear)) {
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
            ControllerViewStateRuntime.set(this, "/availableYears", buildYearOptions(this));
            ControllerViewStateRuntime.set(this, "/compareYearOptions", buildCompareYearOptions(this));
            this._setCompareYearValidation("None", "");
            return this._loadAnalytics("compareYearChanged");
        },

        onLiveChangeAnalyticsCompareYear: function (oEvent) {
            var oInput = oEvent && oEvent.getSource ? oEvent.getSource() : null;
            var sValue = sanitizeYearValue(oEvent && oEvent.getParameter && oEvent.getParameter("value"));
            if (oInput && oInput.getValue && oInput.getValue() !== sValue && oInput.setValue) {
                oInput.setValue(sValue);
            }
            this._setCompareYearValidation("None", "");
        },

        onOpenAnalyticsSelectedYearPicker: function (oEvent) {
            var oSource = oEvent && oEvent.getSource ? oEvent.getSource() : null;
            ensureYearPickerRangeForValue(this, "selectedYear");
            return ensureYearPicker(this).then(function (oPopover) {
                if (oSource) {
                    oPopover.openBy(oSource);
                }
            });
        },

        onOpenAnalyticsCompareYearPicker: function (oEvent) {
            var oSource = oEvent && oEvent.getSource ? oEvent.getSource() : null;
            ensureYearPickerRangeForValue(this, "compareYear");
            return ensureYearPicker(this).then(function (oPopover) {
                if (oSource) {
                    oPopover.openBy(oSource);
                }
            });
        },

        onNavigateAnalyticsYearPickerBack: function () {
            var sTargetField = String(ControllerViewStateRuntime.get(this, "/yearPicker/targetField", "selectedYear") || "selectedYear");
            var iRangeStart = Number(ControllerViewStateRuntime.get(this, "/yearPicker/rangeStart", new Date().getFullYear() - 9) || 0);
            syncYearPickerState(this, sTargetField, iRangeStart - 20);
        },

        onNavigateAnalyticsYearPickerForward: function () {
            var sTargetField = String(ControllerViewStateRuntime.get(this, "/yearPicker/targetField", "selectedYear") || "selectedYear");
            var iRangeStart = Number(ControllerViewStateRuntime.get(this, "/yearPicker/rangeStart", new Date().getFullYear() - 9) || 0);
            syncYearPickerState(this, sTargetField, iRangeStart + 20);
        },

        onSelectAnalyticsYearFromPicker: function (oEvent) {
            var sTargetField = String(ControllerViewStateRuntime.get(this, "/yearPicker/targetField", "selectedYear") || "selectedYear");
            var oSource = oEvent && oEvent.getSource ? oEvent.getSource() : null;
            var sYear = normalizeYearString(oSource && oSource.data && oSource.data("year"));
            if (!sYear) {
                return Promise.resolve();
            }
            if (sTargetField === "compareYear") {
                ControllerViewStateRuntime.set(this, "/compareYear", sYear);
                ControllerViewStateRuntime.set(this, "/availableYears", buildYearOptions(this));
                ControllerViewStateRuntime.set(this, "/compareYearOptions", buildCompareYearOptions(this));
                this._setCompareYearValidation("None", "");
                syncYearPickerState(this, sTargetField, Number(ControllerViewStateRuntime.get(this, "/yearPicker/rangeStart", 0) || 0));
                if (this._oAnalyticsYearPicker) {
                    this._oAnalyticsYearPicker.close();
                }
                return this._loadAnalytics("compareYearPicked");
            }
            ControllerViewStateRuntime.set(this, "/selectedYear", sYear);
            ControllerViewStateRuntime.set(this, "/availableYears", buildYearOptions(this));
            syncCompareYearDefaults(this, sYear);
            this._setCompareYearValidation("None", "");
            syncYearPickerState(this, sTargetField, Number(ControllerViewStateRuntime.get(this, "/yearPicker/rangeStart", 0) || 0));
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
            return applyYearPreset(this, sPreset);
        },

        onDrilldownAnalyticsBuilder: function (oEvent) {
            var sDimension = String(ControllerViewStateRuntime.get(this, "/builderDimension", AnalyticsContracts.BUILDER.FALLBACK_DIMENSION) || AnalyticsContracts.BUILDER.FALLBACK_DIMENSION).trim().toUpperCase();
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
                dimension: AnalyticsContracts.DIMENSIONS.SOURCE
            });
        },

        onDrilldownAnalyticsProfession: function (oEvent) {
            var oPoint = extractDrilldownPayload(oEvent);
            return queueAnalyticsDrilldown(this, "ProfessionText", String((oPoint && oPoint.Profession) || "").trim(), {
                dimension: AnalyticsContracts.DIMENSIONS.PROFESSION
            });
        },

        onDrilldownAnalyticsLpc: function (oEvent) {
            var oPoint = extractDrilldownPayload(oEvent);
            return queueAnalyticsDrilldown(this, "Lpc", String((oPoint && oPoint.LPC) || "").trim(), {
                dimension: AnalyticsContracts.DIMENSIONS.LPC
            });
        },

        onDrilldownAnalyticsLocation: function (oEvent) {
            var oPoint = extractDrilldownPayload(oEvent);
            return queueAnalyticsDrilldown(this, "LocationKey", String((oPoint && oPoint.Location) || "").trim(), {
                dimension: AnalyticsContracts.DIMENSIONS.LOCATION
            });
        },

        onOpenAnalyticsReportDialog: function () {
            return ensureAnalyticsReportDialog(this).then(function (oDialog) {
                oDialog.open();
            });
        },

        onCloseAnalyticsReportDialog: function () {
            if (this._oAnalyticsReportDialog) {
                this._oAnalyticsReportDialog.close();
            }
        },

        onExportAnalyticsReport: function () {
            var oBundle = this.getOwnerComponent().getModel("i18n").getResourceBundle();
            var oViewState = ControllerViewStateRuntime.get(this, "/", {});
            var aRows = AnalyticsExportRows.buildRows(oViewState, oBundle);
            if (!aRows.length) {
                return FeedbackCoordinator.showToast(this, "nothingToExport", [], "warning");
            }
            try {
                ExcelExport.download(buildAnalyticsExportFileName(this), aRows);
                FeedbackCoordinator.showToast(this, "searchExportSuccess", [], "info");
            } catch (_oError) {
                FeedbackCoordinator.showToast(this, "exportFailed", ["analytics"], "error");
            }
            return Promise.resolve(true);
        },

        onCloseAnalytics: function () {
            NavigationIntentService.navigateBackFromAnalytics(this);
        }
    };
});
