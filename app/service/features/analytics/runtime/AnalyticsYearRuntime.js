sap.ui.define([
    "sap/ui/core/Fragment",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectTextResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DialogConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/YearValue"
], function (Fragment, EffectTextResolver, ControllerViewStateRuntime, AnalyticsContracts, AnalyticsUiContracts, DialogContracts, YearValue) {
    "use strict";

    var PATHS = AnalyticsUiContracts.PATHS;
    var YEAR_PICKER_FIELDS = AnalyticsUiContracts.YEAR_PICKER_FIELDS;
    var VALIDATION_STATES = AnalyticsUiContracts.VALIDATION_STATES;
    var TEXT_KEYS = AnalyticsUiContracts.TEXT_KEYS;
    var FRAGMENT_IDS = AnalyticsUiContracts.FRAGMENT_IDS;

    function normalizeYearString(vYear) {
        return YearValue.normalizeYearString(vYear);
    }

    function sanitizeYearValue(vValue) {
        return YearValue.sanitizeYearInput(vValue);
    }

    function isValidYearString(vYear) {
        return !!normalizeYearString(vYear);
    }

    function getFieldPath(sField) {
        return sField === YEAR_PICKER_FIELDS.COMPARE ? PATHS.COMPARE_YEAR : PATHS.SELECTED_YEAR;
    }

    function setYearCollections(oController, sSelectedYearPath, sCompareYearPath) {
        ControllerViewStateRuntime.set(oController, PATHS.AVAILABLE_YEARS, buildYearOptions(oController, sSelectedYearPath, sCompareYearPath));
        ControllerViewStateRuntime.set(oController, PATHS.COMPARE_YEAR_OPTIONS, buildCompareYearOptions(oController, sSelectedYearPath, sCompareYearPath));
    }

    function buildYearOptions(oController, sSelectedYearPath, sCompareYearPath) {
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
            aOptions.push({ key: sYear, text: sYear });
        }

        (ControllerViewStateRuntime.get(oController, PATHS.AVAILABLE_YEARS, []) || []).forEach(function (oYear) {
            pushYear((oYear && (oYear.key || oYear.text)) || "");
        });
        pushYear(ControllerViewStateRuntime.get(oController, sSelectedYearPath, ""));
        pushYear(ControllerViewStateRuntime.get(oController, sCompareYearPath, ""));
        for (; iEndYear >= iStartYear; iEndYear -= 1) {
            pushYear(iEndYear);
        }

        return aOptions.sort(function (aLeft, aRight) {
            return Number(aRight && aRight.key) - Number(aLeft && aLeft.key);
        });
    }

    function buildCompareYearOptions(oController, sSelectedYearPath, sCompareYearPath) {
        var aOptions = buildYearOptions(oController, sSelectedYearPath, sCompareYearPath);
        var iSelectedYear = YearValue.parseYearOrNull(ControllerViewStateRuntime.get(oController, sSelectedYearPath, ""));
        var sDefaultCompareYear = iSelectedYear && iSelectedYear > 1 ? String(iSelectedYear - 1) : "";
        if (sDefaultCompareYear && !aOptions.some(function (oYear) { return oYear && oYear.key === sDefaultCompareYear; })) {
            aOptions.push({ key: sDefaultCompareYear, text: sDefaultCompareYear });
        }
        return aOptions.sort(function (aLeft, aRight) {
            return Number(aRight && aRight.key) - Number(aLeft && aLeft.key);
        });
    }

    function syncCompareYearDefaults(oController, sSelectedYear, sSelectedYearPath, sCompareYearPath) {
        var iSelectedYear = YearValue.parseYearOrNull(sSelectedYear);
        var sDefaultCompareYear = iSelectedYear && iSelectedYear > 1 ? String(iSelectedYear - 1) : "";
        ControllerViewStateRuntime.set(oController, sCompareYearPath, sDefaultCompareYear);
        setYearCollections(oController, sSelectedYearPath, sCompareYearPath);
        return sDefaultCompareYear;
    }

    function applyYearPreset(oController, sPreset, sSelectedYearPath, sCompareYearPath, fnLoadAnalytics) {
        var iCurrentYear = new Date().getFullYear();
        var iSelectedYear = sPreset === AnalyticsContracts.YEAR_PRESETS.PREVIOUS ? (iCurrentYear - 1) : iCurrentYear;
        var sSelectedYear = String(iSelectedYear);
        syncCompareYearDefaults(oController, sSelectedYear, sSelectedYearPath, sCompareYearPath);
        ControllerViewStateRuntime.setMany(oController, {
            "/activeYearPreset": sPreset || AnalyticsContracts.YEAR_PRESETS.CURRENT,
            [PATHS.SELECTED_YEAR]: sSelectedYear
        });
        return fnLoadAnalytics(AnalyticsUiContracts.LOAD_REASONS.YEAR_PRESET_CHANGED);
    }

    function revertInvalidCompareYear(oInput, sStoredCompareYear, fnSetValidation, oController) {
        if (oInput && oInput.setValue) {
            oInput.setValue(sStoredCompareYear);
        }
        fnSetValidation(
            VALIDATION_STATES.ERROR,
            EffectTextResolver.getText(oController, TEXT_KEYS.COMPARE_YEAR_INVALID, [], "Enter a valid four-digit year")
        );
        return Promise.resolve();
    }

    function applyCompareYearChange(oController, oEvent, sStoredCompareYear, fnSetValidation, fnLoadAnalytics) {
        var oInput = oEvent && oEvent.getSource ? oEvent.getSource() : null;
        var sRawYear = sanitizeYearValue(
            (oEvent && oEvent.getParameter && oEvent.getParameter("value")) ||
            (oInput && oInput.getValue && oInput.getValue()) ||
            ""
        );
        var sNormalizedYear = normalizeYearString(sRawYear);

        if (!sNormalizedYear) {
            return revertInvalidCompareYear(oInput, sStoredCompareYear, fnSetValidation, oController);
        }

        ControllerViewStateRuntime.set(oController, PATHS.COMPARE_YEAR, sNormalizedYear);
        setYearCollections(oController, PATHS.SELECTED_YEAR, PATHS.COMPARE_YEAR);
        fnSetValidation(VALIDATION_STATES.NONE, "");
        return fnLoadAnalytics(AnalyticsUiContracts.LOAD_REASONS.COMPARE_YEAR_CHANGED);
    }

    function buildYearPickerItems(iRangeStart, sTargetField, oController) {
        var aItems = [];
        var iYear;
        var sActiveYear = String(ControllerViewStateRuntime.get(oController, getFieldPath(sTargetField), "") || "").trim();

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
        var oPatch = {};
        oPatch[PATHS.YEAR_PICKER_TARGET_FIELD] = sTargetField;
        oPatch[PATHS.YEAR_PICKER_RANGE_START] = iSafeStart;
        oPatch[PATHS.YEAR_PICKER_RANGE_END] = iSafeStart + 19;
        oPatch[PATHS.YEAR_PICKER_RANGE_LABEL] = String(iSafeStart) + " - " + String(iSafeStart + 19);
        oPatch[PATHS.YEAR_PICKER_ITEMS] = buildYearPickerItems(iSafeStart, sTargetField, oController);
        ControllerViewStateRuntime.setMany(oController, oPatch);
    }

    function ensureYearPickerRangeForValue(oController, sTargetField) {
        var sYear = normalizeYearString(ControllerViewStateRuntime.get(oController, getFieldPath(sTargetField), ""));
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
            id: oController.getView().createId(FRAGMENT_IDS.YEAR_PICKER),
            name: DialogContracts.getFragmentName(DialogContracts.IDS.ANALYTICS_YEAR_PICKER),
            controller: oController
        }).then(function (oPopover) {
            oController.getView().addDependent(oPopover);
            oController._oAnalyticsYearPicker = oPopover;
            return oPopover;
        });
        return oController._pAnalyticsYearPicker;
    }

    return {
        applyCompareYearChange: applyCompareYearChange,
        applyYearPreset: applyYearPreset,
        buildCompareYearOptions: buildCompareYearOptions,
        buildYearOptions: buildYearOptions,
        ensureYearPicker: ensureYearPicker,
        ensureYearPickerRangeForValue: ensureYearPickerRangeForValue,
        isValidYearString: isValidYearString,
        normalizeYearString: normalizeYearString,
        sanitizeYearValue: sanitizeYearValue,
        syncCompareYearDefaults: syncCompareYearDefaults,
        syncYearPickerState: syncYearPickerState
    };
});
