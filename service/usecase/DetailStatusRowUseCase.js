sap.ui.define([
    "sap_ui5/util/ChecklistUiState"
], function (ChecklistUiState) {
    "use strict";


    function formatHumanDateLong(sDate) {
        if (!sDate) {
            return "-";
        }
        var oDate = new Date(sDate);
        if (Number.isNaN(oDate.getTime()) && /^\d{4}-\d{2}-\d{2}$/.test(sDate)) {
            oDate = new Date(sDate + "T00:00:00");
        }
        if (Number.isNaN(oDate.getTime())) {
            return sDate;
        }
        return oDate.toLocaleDateString(undefined, {
            weekday: "short",
            day: "2-digit",
            month: "short",
            year: "numeric"
        });
    }

    function formatInfoCardValue(sKey, mValues) {
        if (sKey === "date") {
            return formatHumanDateLong(mValues.date);
        }
        if (sKey === "time") {
            return mValues.time || "-";
        }
        if (sKey === "timezone") {
            return mValues.timezone || "-";
        }
        if (sKey === "equipment") {
            return mValues.equipment || "-";
        }
        if (sKey === "observer") {
            return mValues.observer || "-";
        }
        if (sKey === "observed") {
            return mValues.observed || "-";
        }
        if (sKey === "lpc") {
            return mValues.lpcText || "-";
        }
        if (sKey === "profession") {
            return mValues.profText || "-";
        }

        if (mValues.locationName && mValues.locationText && mValues.locationName !== mValues.locationText) {
            return mValues.locationName + " — " + mValues.locationText;
        }
        return mValues.locationName || mValues.locationText || "-";
    }

    function shouldApplyStatusChange(sCurrentStatus, sTargetStatus) {
        return !ChecklistUiState.isSameStatus(sCurrentStatus, sTargetStatus);
    }

    function requiresIntegrationConfirmation(oRoot) {
        return !!((oRoot || {}).this_is_integration_data);
    }

    function shouldSyncAfterDeleteResult(oResult) {
        return !!(oResult && oResult.deleted);
    }

    function resolveExpandedDialogMeta(sType) {
        return sType === "checks"
            ? { prop: "_pChecksDialog", fragment: "sap_ui5.view.fragment.ChecksExpandedDialog" }
            : { prop: "_pBarriersDialog", fragment: "sap_ui5.view.fragment.BarriersExpandedDialog" };
    }

    return {
        formatHumanDateLong: formatHumanDateLong,
        formatInfoCardValue: formatInfoCardValue,
        shouldApplyStatusChange: shouldApplyStatusChange,
        requiresIntegrationConfirmation: requiresIntegrationConfirmation,
        shouldSyncAfterDeleteResult: shouldSyncAfterDeleteResult,
        resolveExpandedDialogMeta: resolveExpandedDialogMeta
    };
});
