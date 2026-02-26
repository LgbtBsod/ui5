sap.ui.define([], function () {
    "use strict";

    function getBundleText(mArgs) {
        var oBundle = mArgs && mArgs.bundle;
        var sKey = mArgs && mArgs.key;
        var aParams = mArgs && mArgs.params;

        if (!oBundle || typeof oBundle.getText !== "function" || !sKey) {
            return "";
        }

        try {
            return oBundle.getText(sKey, aParams);
        } catch (_e) {
            return "";
        }
    }

    function presentToastMessage(mArgs) {
        var fnShowToast = mArgs && mArgs.showToast;
        if (typeof fnShowToast !== "function") {
            return false;
        }

        var sText = getBundleText({
            bundle: mArgs && mArgs.bundle,
            key: mArgs && mArgs.messageKey,
            params: mArgs && mArgs.params
        }) || (mArgs && mArgs.fallbackText) || "";

        if (!sText) {
            return false;
        }

        try {
            fnShowToast(sText);
            return true;
        } catch (_e) {
            return false;
        }
    }

    function presentCopyMissingSelection(mArgs) {
        return presentToastMessage({
            bundle: mArgs && mArgs.bundle,
            showToast: mArgs && mArgs.showToast,
            messageKey: "nothingToCopy",
            fallbackText: "Select a checklist to copy."
        });
    }

    function presentMissingChecklistId(mArgs) {
        return presentToastMessage({
            bundle: mArgs && mArgs.bundle,
            showToast: mArgs && mArgs.showToast,
            messageKey: "checklistIdMissing",
            fallbackText: "Checklist id not found"
        });
    }


    function presentExportIntentResult(mArgs) {
        var oResult = mArgs && mArgs.result;
        if (!oResult || oResult.ok) {
            return {
                ok: false,
                reason: "no_presentation_needed"
            };
        }

        if (oResult.reason === "disabled") {
            var bDisabledPresented = presentToastMessage({
                bundle: mArgs && mArgs.bundle,
                showToast: mArgs && mArgs.showToast,
                messageKey: "exportEmpty",
                fallbackText: "No data to export"
            });
            return {
                ok: bDisabledPresented,
                reason: bDisabledPresented ? "presented_disabled" : "presentation_failed_disabled",
                messageKey: "exportEmpty"
            };
        }

        if (oResult.reason === "run_error" || oResult.reason === "missing_run_export") {
            var sMessage = (oResult.error && oResult.error.message) || "Unknown error";
            var bErrorPresented = presentToastMessage({
                bundle: mArgs && mArgs.bundle,
                showToast: mArgs && mArgs.showToast,
                messageKey: "exportFailed",
                params: [sMessage],
                fallbackText: "Export failed: " + sMessage
            });
            return {
                ok: bErrorPresented,
                reason: bErrorPresented ? "presented_error" : "presentation_failed_error",
                messageKey: "exportFailed"
            };
        }

        return {
            ok: false,
            reason: "unknown_result_reason"
        };
    }

    function presentDeleteFlowResult(mArgs) {
        var oResult = mArgs && mArgs.result;

        if (oResult && oResult.ok) {
            return presentToastMessage({
                bundle: mArgs && mArgs.bundle,
                showToast: mArgs && mArgs.showToast,
                messageKey: "deleted",
                fallbackText: "Checklist deleted."
            });
        }

        if (oResult && oResult.reason === "missing_id") {
            return presentToastMessage({
                bundle: mArgs && mArgs.bundle,
                showToast: mArgs && mArgs.showToast,
                messageKey: "nothingToDelete",
                fallbackText: "Select a checklist to delete."
            });
        }

        var sMessage = (oResult && oResult.error && oResult.error.message) || "Unknown error";
        return presentToastMessage({
            bundle: mArgs && mArgs.bundle,
            showToast: mArgs && mArgs.showToast,
            messageKey: "deleteFailed",
            params: [sMessage],
            fallbackText: "Delete failed: " + sMessage
        });
    }

    return {
        getBundleText: getBundleText,
        presentToastMessage: presentToastMessage,
        presentCopyMissingSelection: presentCopyMissingSelection,
        presentMissingChecklistId: presentMissingChecklistId,
        presentDeleteFlowResult: presentDeleteFlowResult,
        presentExportIntentResult: presentExportIntentResult
    };
});
