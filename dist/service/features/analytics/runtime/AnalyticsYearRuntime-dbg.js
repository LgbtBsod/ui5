sap.ui.define([
    "sap/ui/core/Fragment",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectTextResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/DialogContracts"
], function (Fragment, EffectTextResolver, ControllerViewStateRuntime, AnalyticsContracts, DialogContracts) {
    "use strict";

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

    function sanitizeYearValue(vValue) {
        return String(vValue || "").replace(/\D+/g, "").slice(0, 4);
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

        (ControllerViewStateRuntime.get(oController, "/availableYears", []) || []).forEach(function (oYear) {
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
        var sDefaultCompareYear = normalizeYearString(Number(ControllerViewStateRuntime.get(oController, sSelectedYearPath, 0)) - 1);
        if (sDefaultCompareYear && !aOptions.some(function (oYear) { return oYear && oYear.key === sDefaultCompareYear; })) {
            aOptions.push({ key: sDefaultCompareYear, text: sDefaultCompareYear });
        }
        return aOptions.sort(function (aLeft, aRight) {
            return Number(aRight && aRight.key) - Number(aLeft && aLeft.key);
        });
    }

    function syncCompareYearDefaults(oController, sSelectedYear, sSelectedYearPath, sCompareYearPath) {
        var iSelectedYear = Number(String(sSelectedYear || "").trim());
        var sDefaultCompareYear = iSelectedYear > 0 ? String(iSelectedYear - 1) : "";
        ControllerViewStateRuntime.set(oController, sCompareYearPath, sDefaultCompareYear);
        ControllerViewStateRuntime.set(oController, "/availableYears", buildYearOptions(oController, sSelectedYearPath, sCompareYearPath));
        ControllerViewStateRuntime.set(oController, "/compareYearOptions", buildCompareYearOptions(oController, sSelectedYearPath, sCompareYearPath));
        return sDefaultCompareYear;
    }

    function applyYearPreset(oController, sPreset, sSelectedYearPath, sCompareYearPath, fnLoadAnalytics) {
        var iCurrentYear = new Date().getFullYear();
        var iSelectedYear = sPreset === AnalyticsContracts.YEAR_PRESETS.PREVIOUS ? (iCurrentYear - 1) : iCurrentYear;
        var sSelectedYear = String(iSelectedYear);
        syncCompareYearDefaults(oController, sSelectedYear, sSelectedYearPath, sCompareYearPath);
        ControllerViewStateRuntime.setMany(oController, {
            "/selectedYear": sSelectedYear,
            "/activeYearPreset": sPreset || AnalyticsContracts.YEAR_PRESETS.CURRENT
        });
        return fnLoadAnalytics("yearPresetChanged");
    }

    function applyCompareYearChange(oController, oEvent, sStoredCompareYear, fnSetValidation, fnLoadAnalytics) {
        var oInput = oEvent && oEvent.getSource ? oEvent.getSource() : null;
        var sRawYear = sanitizeYearValue(
            oEvent && oEvent.getParameter && oEvent.getParameter("value") ||
            oInput && oInput.getValue && oInput.getValue() ||
            ""
        );
        var iYear;
        if (!isValidYearString(sRawYear)) {
            if (oInput && oInput.setValue) {
                oInput.setValue(sStoredCompareYear);
            }
            fnSetValidation("Error", EffectTextResolver.getText(oController, "analyticsCompareYearInvalid", [], "Enter a valid four-digit year"));
            return Promise.resolve();
        }
        iYear = Number(sRawYear);
        if (!Number.isFinite(iYear) || iYear <= 0) {
            if (oInput && oInput.setValue) {
                oInput.setValue(sStoredCompareYear);
            }
            fnSetValidation("Error", EffectTextResolver.getText(oController, "analyticsCompareYearInvalid", [], "Enter a valid four-digit year"));
            return Promise.resolve();
        }
        ControllerViewStateRuntime.set(oController, "/compareYear", String(iYear));
        ControllerViewStateRuntime.set(oController, "/availableYears", buildYearOptions(oController, "/selectedYear", "/compareYear"));
        ControllerViewStateRuntime.set(oController, "/compareYearOptions", buildCompareYearOptions(oController, "/selectedYear", "/compareYear"));
        fnSetValidation("None", "");
        return fnLoadAnalytics("compareYearChanged");
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
