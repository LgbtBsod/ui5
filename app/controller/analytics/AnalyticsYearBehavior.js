sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsYearRuntime"
], function (ControllerViewStateRuntime, AnalyticsYearRuntime) {
    "use strict";

    function extractYearValueFromEvent(oEvent) {
        var oSource = oEvent && oEvent.getSource ? oEvent.getSource() : null;
        var oSelectedItem = oEvent && oEvent.getParameter && oEvent.getParameter("selectedItem");
        return AnalyticsYearRuntime.normalizeYearString(
            (oSelectedItem && oSelectedItem.getKey && oSelectedItem.getKey()) ||
            (oEvent && oEvent.getParameter && (oEvent.getParameter("selectedKey") || oEvent.getParameter("value"))) ||
            (oSource && oSource.getSelectedKey && oSource.getSelectedKey()) ||
            ""
        );
    }

    function applyYearSelection(oController, sYearPath, sYear, sSelectedYearPath, sCompareYearPath, fnSetCompareYearValidation, fnLoadAnalytics, sReason) {
        if (!sYear) {
            return Promise.resolve();
        }
        ControllerViewStateRuntime.set(oController, sYearPath, sYear);
        ControllerViewStateRuntime.set(oController, "/availableYears", AnalyticsYearRuntime.buildYearOptions(oController, sSelectedYearPath, sCompareYearPath));
        if (sYearPath === sSelectedYearPath) {
            AnalyticsYearRuntime.syncCompareYearDefaults(oController, sYear, sSelectedYearPath, sCompareYearPath);
        } else {
            ControllerViewStateRuntime.set(oController, "/compareYearOptions", AnalyticsYearRuntime.buildCompareYearOptions(oController, sSelectedYearPath, sCompareYearPath));
        }
        fnSetCompareYearValidation("None", "");
        return fnLoadAnalytics(sReason);
    }

    function selectAnalyticsYear(oController, oEvent, sSelectedYearPath, sCompareYearPath, fnSetCompareYearValidation, fnLoadAnalytics) {
        var oSource = oEvent && oEvent.getSource ? oEvent.getSource() : null;
        var sYear = extractYearValueFromEvent(oEvent);
        if (!sYear) {
            if (oSource && oSource.setValue) {
                oSource.setValue(String(ControllerViewStateRuntime.get(oController, sSelectedYearPath, "") || ""));
            }
            return Promise.resolve();
        }
        return applyYearSelection(oController, sSelectedYearPath, sYear, sSelectedYearPath, sCompareYearPath, fnSetCompareYearValidation, fnLoadAnalytics, "yearChanged");
    }

    return {
        onApplyAnalyticsYearPreset: function (oController, oEvent, sSelectedYearPath, sCompareYearPath, fnLoadAnalytics) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            var sPreset = String((oSource && oSource.data && oSource.data("preset")) || "").trim().toUpperCase();
            if (!sPreset) {
                return Promise.resolve();
            }
            return AnalyticsYearRuntime.applyYearPreset(oController, sPreset, sSelectedYearPath, sCompareYearPath, fnLoadAnalytics);
        },

        onChangeAnalyticsCompareYear: function (oController, oEvent, fnSetCompareYearValidation, fnLoadAnalytics) {
            var sStoredCompareYear = String(ControllerViewStateRuntime.get(oController, "/compareYear", "") || "").trim();
            return AnalyticsYearRuntime.applyCompareYearChange(oController, oEvent, sStoredCompareYear, fnSetCompareYearValidation, fnLoadAnalytics);
        },

        onLiveChangeAnalyticsCompareYear: function (oController, oEvent, fnSetCompareYearValidation) {
            var oInput = oEvent && oEvent.getSource ? oEvent.getSource() : null;
            var sValue = AnalyticsYearRuntime.sanitizeYearValue(oEvent && oEvent.getParameter && oEvent.getParameter("value"));
            if (oInput && oInput.getValue && oInput.getValue() !== sValue && oInput.setValue) {
                oInput.setValue(sValue);
            }
            fnSetCompareYearValidation("None", "");
        },

        onLiveChangeAnalyticsYear: function (oEvent) {
            var oInput = oEvent && oEvent.getSource ? oEvent.getSource() : null;
            var sValue = AnalyticsYearRuntime.sanitizeYearValue(oEvent && oEvent.getParameter && oEvent.getParameter("value"));
            if (oInput && oInput.getValue && oInput.getValue() !== sValue && oInput.setValue) {
                oInput.setValue(sValue);
            }
        },

        onNavigateAnalyticsYearPickerBack: function (oController) {
            var sTargetField = String(ControllerViewStateRuntime.get(oController, "/yearPicker/targetField", "selectedYear") || "selectedYear");
            var iRangeStart = Number(ControllerViewStateRuntime.get(oController, "/yearPicker/rangeStart", new Date().getFullYear() - 9) || 0);
            AnalyticsYearRuntime.syncYearPickerState(oController, sTargetField, iRangeStart - 20);
        },

        onNavigateAnalyticsYearPickerForward: function (oController) {
            var sTargetField = String(ControllerViewStateRuntime.get(oController, "/yearPicker/targetField", "selectedYear") || "selectedYear");
            var iRangeStart = Number(ControllerViewStateRuntime.get(oController, "/yearPicker/rangeStart", new Date().getFullYear() - 9) || 0);
            AnalyticsYearRuntime.syncYearPickerState(oController, sTargetField, iRangeStart + 20);
        },

        onOpenAnalyticsCompareYearPicker: function (oController, oEvent) {
            var oSource = oEvent && oEvent.getSource ? oEvent.getSource() : null;
            AnalyticsYearRuntime.ensureYearPickerRangeForValue(oController, "compareYear");
            return AnalyticsYearRuntime.ensureYearPicker(oController).then(function (oPopover) {
                if (oSource) {
                    oPopover.openBy(oSource);
                }
            });
        },

        onOpenAnalyticsSelectedYearPicker: function (oController, oEvent) {
            var oSource = oEvent && oEvent.getSource ? oEvent.getSource() : null;
            AnalyticsYearRuntime.ensureYearPickerRangeForValue(oController, "selectedYear");
            return AnalyticsYearRuntime.ensureYearPicker(oController).then(function (oPopover) {
                if (oSource) {
                    oPopover.openBy(oSource);
                }
            });
        },

        onSelectAnalyticsSource: function (oController, oEvent, fnLoadAnalytics) {
            var sSource = String(
                oEvent && oEvent.getParameter && oEvent.getParameter("selectedKey") ||
                oEvent && oEvent.getSource && oEvent.getSource().getSelectedKey && oEvent.getSource().getSelectedKey() ||
                ""
            ).trim().toUpperCase();
            if (!sSource) {
                return Promise.resolve();
            }
            ControllerViewStateRuntime.set(oController, "/selectedSource", sSource);
            return fnLoadAnalytics("sourceChanged");
        },

        onSelectAnalyticsYear: function (oController, oEvent, sSelectedYearPath, sCompareYearPath, fnSetCompareYearValidation, fnLoadAnalytics) {
            return selectAnalyticsYear(oController, oEvent, sSelectedYearPath, sCompareYearPath, fnSetCompareYearValidation, fnLoadAnalytics);
        },

        onSelectAnalyticsYearFromPicker: function (oController, oEvent, sSelectedYearPath, sCompareYearPath, fnSetCompareYearValidation, fnLoadAnalytics) {
            var sTargetField = String(ControllerViewStateRuntime.get(oController, "/yearPicker/targetField", "selectedYear") || "selectedYear");
            var oSource = oEvent && oEvent.getSource ? oEvent.getSource() : null;
            var sYear = AnalyticsYearRuntime.normalizeYearString(oSource && oSource.data && oSource.data("year"));
            var sTargetPath = sTargetField === "compareYear" ? sCompareYearPath : sSelectedYearPath;
            var sReason = sTargetField === "compareYear" ? "compareYearPicked" : "yearPicked";
            if (!sYear) {
                return Promise.resolve();
            }
            AnalyticsYearRuntime.syncYearPickerState(oController, sTargetField, Number(ControllerViewStateRuntime.get(oController, "/yearPicker/rangeStart", 0) || 0));
            if (oController._oAnalyticsYearPicker) {
                oController._oAnalyticsYearPicker.close();
            }
            return applyYearSelection(oController, sTargetPath, sYear, sSelectedYearPath, sCompareYearPath, fnSetCompareYearValidation, fnLoadAnalytics, sReason);
        }
    };
});
