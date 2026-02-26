sap.ui.define([], function () {
    "use strict";

    function validationState(bShown, bMissing) {
        return (bShown && bMissing) ? "Error" : "None";
    }

    function validationText(bShown, bMissing, oBundle) {
        if (!(bShown && bMissing)) {
            return "";
        }
        return oBundle ? oBundle.getText("requiredFieldHint") : "";
    }

    function booleanResultText(vValue, oBundle) {
        if (vValue === true) {
            return oBundle ? oBundle.getText("statusOk") : "OK";
        }
        if (vValue === false) {
            return oBundle ? oBundle.getText("statusFailed") : "Failed";
        }
        return "-";
    }

    function booleanResultState(vValue) {
        if (vValue === true) {
            return "Success";
        }
        if (vValue === false) {
            return "Error";
        }
        return "None";
    }

    function lifecycleStatusText(sStatus, oBundle) {
        switch (String(sStatus || "").toUpperCase()) {
            case "DRAFT": return oBundle ? oBundle.getText("statusDraft") : "DRAFT";
            case "REGISTERED": return oBundle ? oBundle.getText("statusRegistered") : "REGISTERED";
            case "CLOSED": return oBundle ? oBundle.getText("statusClosed") : "CLOSED";
            default: return sStatus || "-";
        }
    }

    function draftStateText(bDirty, oBundle) {
        if (!oBundle) {
            return bDirty ? "Changed" : "Saved";
        }
        return bDirty ? oBundle.getText("detailDraftChanged") : oBundle.getText("detailDraftClean");
    }

    function draftStateState(bDirty) {
        return bDirty ? "Warning" : "Success";
    }

    function lifecycleStatusState(sStatus) {
        switch (String(sStatus || "").toUpperCase()) {
            case "REGISTERED": return "Warning";
            case "CLOSED": return "Success";
            default: return "Information";
        }
    }


    function lockOperationText(sOperationText, sMode, oBundle) {
        if (sOperationText) {
            return sOperationText;
        }
        if (String(sMode || "").toUpperCase() === "EDIT") {
            return oBundle ? oBundle.getText("modeEdit") : "EDIT";
        }
        return oBundle ? oBundle.getText("modeRead") : "READ";
    }

    function lockOperationState(sOperationState, sMode) {
        var sState = String(sOperationState || "").toUpperCase();
        if (sState === "SUCCESS") {
            return "Success";
        }
        if (sState === "ERROR") {
            return "Error";
        }
        if (String(sMode || "").toUpperCase() === "EDIT") {
            return "Success";
        }
        return "Information";
    }

    function passedTotal(aRows) {
        var aSafeRows = aRows || [];
        var iTotal = aSafeRows.length;
        if (!iTotal) {
            return "";
        }
        var iPassed = aSafeRows.filter(function (oRow) { return !!(oRow && oRow.result); }).length;
        return iPassed + "/" + iTotal;
    }

    return {
        validationState: validationState,
        validationText: validationText,
        booleanResultText: booleanResultText,
        booleanResultState: booleanResultState,
        lifecycleStatusText: lifecycleStatusText,
        draftStateText: draftStateText,
        draftStateState: draftStateState,
        lifecycleStatusState: lifecycleStatusState,
        lockOperationText: lockOperationText,
        lockOperationState: lockOperationState,
        passedTotal: passedTotal
    };
});
