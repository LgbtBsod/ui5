sap.ui.define([], function () {
    "use strict";

    function resolveMessageSpec(oResult) {
        var sReason = oResult && oResult.reason;
        switch (String(sReason || "")) {
            case "reloaded":
            case "legacy_reload":
                return { key: "saveConflictReloaded", params: null };
            case "overwritten":
            case "legacy_overwrite":
                return { key: "saveConflictOverwritten", params: null };
            case "cancelled":
                return { key: "saveConflictCancelled", params: null };
            case "missing_reload_handler":
            case "missing_overwrite_handler":
                return { key: "saveConflictHandlerMissing", params: null };
            case "backend_error": {
                var sMessage = (oResult && oResult.error && oResult.error.message) || "Unknown error";
                return { key: "genericOperationFailed", params: [sMessage] };
            }
            default:
                return { key: "", params: null };
        }
    }

    function resolveMessageKey(sReason) {
        return resolveMessageSpec({ reason: sReason }).key;
    }

    function presentOutcome(mArgs) {
        var oResult = mArgs && mArgs.result;
        var oSpec = resolveMessageSpec(oResult);
        var sMessageKey = oSpec.key;
        if (!sMessageKey) {
            return { ok: false, reason: "unknown_outcome", messageKey: "", message: "" };
        }

        var fnShowToast = mArgs && mArgs.showToast;
        var oBundle = mArgs && mArgs.bundle;
        if (!oBundle || typeof oBundle.getText !== "function") {
            return { ok: false, reason: "missing_bundle_adapter", messageKey: sMessageKey, message: "" };
        }
        if (typeof fnShowToast !== "function") {
            return { ok: false, reason: "missing_toast_adapter", messageKey: sMessageKey, message: "" };
        }

        try {
            var sMessage = oBundle.getText(sMessageKey, oSpec.params || undefined);
            fnShowToast(sMessage);
            return { ok: true, reason: "presented", messageKey: sMessageKey, message: sMessage };
        } catch (_e) {
            return { ok: false, reason: "toast_error", messageKey: sMessageKey, message: "" };
        }
    }

    return {
        resolveMessageSpec: resolveMessageSpec,
        resolveMessageKey: resolveMessageKey,
        presentOutcome: presentOutcome
    };
});
