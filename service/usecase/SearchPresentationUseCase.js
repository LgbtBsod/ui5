sap.ui.define([], function () {
    "use strict";

    function normalizeMaxRowsInput(sRawValue) {
        var sRaw = String(sRawValue || "").trim();
        if (!sRaw) {
            return "";
        }
        var iParsed = Math.max(1, Math.min(9999, Number(sRaw) || 0));
        return iParsed ? String(iParsed) : "";
    }

    function formatOverallResultText(vResult, sLifecycleStatus, oBundle) {
        if (vResult === true) {
            return oBundle.getText("statusOk");
        }
        if (vResult === false) {
            return oBundle.getText("statusFailed");
        }
        return sLifecycleStatus || "-";
    }

    function formatOverallResultState(vResult, sLifecycleStatus) {
        if (vResult === true) {
            return "Success";
        }
        if (vResult === false) {
            return "Error";
        }
        switch (String(sLifecycleStatus || "").toUpperCase()) {
            case "CLOSED": return "Success";
            case "REGISTERED": return "Warning";
            default: return "Information";
        }
    }

    function formatStatus(sStatus) {
        switch (String(sStatus || "").toUpperCase()) {
            case "SUCCESS":
            case "CLOSED": return "Success";
            case "WARNING":
            case "REGISTERED": return "Warning";
            case "CRITICAL":
            case "ERROR": return "Error";
            default: return "None";
        }
    }

    return {
        normalizeMaxRowsInput: normalizeMaxRowsInput,
        formatOverallResultText: formatOverallResultText,
        formatOverallResultState: formatOverallResultState,
        formatStatus: formatStatus
    };
});
