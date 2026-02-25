sap.ui.define([
    "sap_ui5/util/ChecklistUiState"
], function (ChecklistUiState) {
    "use strict";

    function formatInfoCardValue(sKey, mValues) {
        if (sKey === "date") {
            return mValues.date || "-";
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
        formatInfoCardValue: formatInfoCardValue,
        shouldApplyStatusChange: shouldApplyStatusChange,
        requiresIntegrationConfirmation: requiresIntegrationConfirmation,
        shouldSyncAfterDeleteResult: shouldSyncAfterDeleteResult,
        resolveExpandedDialogMeta: resolveExpandedDialogMeta
    };
});
