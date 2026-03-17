sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsUiContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsYearRuntime"
], function (ControllerViewStateRuntime, AnalyticsUiContracts, AnalyticsYearRuntime) {
    "use strict";

    var PATHS = AnalyticsUiContracts.PATHS;
    var YEAR_PICKER_FIELDS = AnalyticsUiContracts.YEAR_PICKER_FIELDS;
    var VALIDATION_STATES = AnalyticsUiContracts.VALIDATION_STATES;
    var LOAD_REASONS = AnalyticsUiContracts.LOAD_REASONS;

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

    return {
        onApplyAnalyticsYearPreset: function (oController, oEvent, sSelectedYearPath, sCompareYearPath, fnLoadAnalytics) {
            var oSource = getEventSource(oEvent);
            var sPreset = String((oSource && oSource.data && oSource.data("preset")) || "").trim().toUpperCase();
            if (!sPreset) {
                return Promise.resolve();
            }
            return AnalyticsYearRuntime.applyYearPreset(oController, sPreset, sSelectedYearPath, sCompareYearPath, fnLoadAnalytics);
        },

        onChangeAnalyticsCompareYear: function (oController, oEvent, fnSetCompareYearValidation, fnLoadAnalytics) {
            var sStoredCompareYear = String(ControllerViewStateRuntime.get(oController, PATHS.COMPARE_YEAR, "") || "").trim();
            return AnalyticsYearRuntime.applyCompareYearChange(oController, oEvent, sStoredCompareYear, fnSetCompareYearValidation, fnLoadAnalytics);
        },

        onLiveChangeAnalyticsCompareYear: function (oController, oEvent, fnSetCompareYearValidation) {
            var oInput = getEventSource(oEvent);
            var sValue = AnalyticsYearRuntime.sanitizeYearValue(getEventParameter(oEvent, "value"));
            if (oInput && oInput.getValue && oInput.getValue() !== sValue && oInput.setValue) {
                oInput.setValue(sValue);
            }
            clearCompareYearValidation(fnSetCompareYearValidation);
        },

        onLiveChangeAnalyticsYear: function (oEvent) {
            var oInput = getEventSource(oEvent);
            var sValue = AnalyticsYearRuntime.sanitizeYearValue(getEventParameter(oEvent, "value"));
            if (oInput && oInput.getValue && oInput.getValue() !== sValue && oInput.setValue) {
                oInput.setValue(sValue);
            }
        },

        onNavigateAnalyticsYearPickerBack: function (oController) {
            var sTargetField = String(ControllerViewStateRuntime.get(oController, PATHS.YEAR_PICKER_TARGET_FIELD, YEAR_PICKER_FIELDS.SELECTED) || YEAR_PICKER_FIELDS.SELECTED);
            var iRangeStart = Number(ControllerViewStateRuntime.get(oController, PATHS.YEAR_PICKER_RANGE_START, new Date().getFullYear() - 9) || 0);
            AnalyticsYearRuntime.syncYearPickerState(oController, sTargetField, iRangeStart - 20);
        },

        onNavigateAnalyticsYearPickerForward: function (oController) {
            var sTargetField = String(ControllerViewStateRuntime.get(oController, PATHS.YEAR_PICKER_TARGET_FIELD, YEAR_PICKER_FIELDS.SELECTED) || YEAR_PICKER_FIELDS.SELECTED);
            var iRangeStart = Number(ControllerViewStateRuntime.get(oController, PATHS.YEAR_PICKER_RANGE_START, new Date().getFullYear() - 9) || 0);
            AnalyticsYearRuntime.syncYearPickerState(oController, sTargetField, iRangeStart + 20);
        },

        onOpenAnalyticsCompareYearPicker: function (oController, oEvent) {
            return openYearPicker(oController, oEvent, YEAR_PICKER_FIELDS.COMPARE);
        },

        onOpenAnalyticsSelectedYearPicker: function (oController, oEvent) {
            return openYearPicker(oController, oEvent, YEAR_PICKER_FIELDS.SELECTED);
        },

        onSelectAnalyticsSource: function (oController, oEvent, fnLoadAnalytics) {
            var oSource = getEventSource(oEvent);
            var sSource = String(getEventParameter(oEvent, "selectedKey") || (oSource && oSource.getSelectedKey && oSource.getSelectedKey()) || "").trim().toUpperCase();
            if (!sSource) {
                return Promise.resolve();
            }
            ControllerViewStateRuntime.set(oController, PATHS.SELECTED_SOURCE, sSource);
            return fnLoadAnalytics(LOAD_REASONS.SOURCE_CHANGED);
        },

        onSelectAnalyticsYear: function (oController, oEvent, sSelectedYearPath, sCompareYearPath, fnSetCompareYearValidation, fnLoadAnalytics) {
            var oSource = getEventSource(oEvent);
            var sYear = extractYearValueFromEvent(oEvent);
            if (!sYear) {
                if (oSource && oSource.setValue) {
                    oSource.setValue(String(ControllerViewStateRuntime.get(oController, sSelectedYearPath, "") || ""));
                }
                return Promise.resolve();
            }
            return applyYearSelection(oController, sSelectedYearPath, sYear, sSelectedYearPath, sCompareYearPath, fnSetCompareYearValidation, fnLoadAnalytics, LOAD_REASONS.YEAR_CHANGED);
        },

        onSelectAnalyticsYearFromPicker: function (oController, oEvent, sSelectedYearPath, sCompareYearPath, fnSetCompareYearValidation, fnLoadAnalytics) {
            var sTargetField = String(ControllerViewStateRuntime.get(oController, PATHS.YEAR_PICKER_TARGET_FIELD, YEAR_PICKER_FIELDS.SELECTED) || YEAR_PICKER_FIELDS.SELECTED);
            var oSource = getEventSource(oEvent);
            var sYear = AnalyticsYearRuntime.normalizeYearString(oSource && oSource.data && oSource.data("year"));
            var sTargetPath = sTargetField === YEAR_PICKER_FIELDS.COMPARE ? sCompareYearPath : sSelectedYearPath;
            var sReason = sTargetField === YEAR_PICKER_FIELDS.COMPARE ? LOAD_REASONS.COMPARE_YEAR_PICKED : LOAD_REASONS.YEAR_PICKED;
            var iRangeStart = Number(ControllerViewStateRuntime.get(oController, PATHS.YEAR_PICKER_RANGE_START, 0) || 0);
            if (!sYear) {
                return Promise.resolve();
            }
            AnalyticsYearRuntime.syncYearPickerState(oController, sTargetField, iRangeStart);
            if (oController._oAnalyticsYearPicker) {
                oController._oAnalyticsYearPicker.close();
            }
            return applyYearSelection(oController, sTargetPath, sYear, sSelectedYearPath, sCompareYearPath, fnSetCompareYearValidation, fnLoadAnalytics, sReason);
        }
    };
});
