sap.ui.define([], function () {
    "use strict";

    function getBundleText(mArgs) {
        var oBundle = mArgs && mArgs.bundle;
        var sKey = mArgs && mArgs.key;
        var aParams = mArgs && mArgs.params;
        var sFallback = (mArgs && mArgs.fallbackText) || "";

        if (!oBundle || typeof oBundle.getText !== "function" || !sKey) {
            return sFallback;
        }

        try {
            return oBundle.getText(sKey, aParams);
        } catch (_e) {
            return sFallback;
        }
    }

    function resolveRetryOutcomePresentation(mArgs) {
        var oResult = mArgs && mArgs.result;
        var oError = oResult && oResult.error;
        var sErrorMessage = (oError && oError.message) || "Unknown error";

        if (oResult && oResult.ok) {
            var iCount = Array.isArray(oResult.rows) ? oResult.rows.length : 0;
            return {
                messageKey: iCount > 0 ? "retryLoadSuccess" : "retryLoadEmpty",
                messageParams: iCount > 0 ? [iCount] : null,
                fallbackText: iCount > 0
                    ? "Data reloaded: " + iCount + " rows"
                    : "No rows loaded.",
                shouldSetBanner: false,
                bannerText: ""
            };
        }

        if (oResult && oResult.reason === "missing_loader") {
            return {
                messageKey: "retryLoadUnavailable",
                messageParams: null,
                fallbackText: "Retry is unavailable.",
                shouldSetBanner: true,
                bannerText: "Retry is unavailable."
            };
        }

        if (oResult && oResult.reason === "empty_rows") {
            return {
                messageKey: "retryLoadEmptyError",
                messageParams: null,
                fallbackText: "No rows returned from backend.",
                shouldSetBanner: true,
                bannerText: "No rows returned from backend."
            };
        }

        if (oResult && oResult.reason === "error" && Number(oResult.attempts) > 1) {
            return {
                messageKey: "retryLoadFailedAfterRetries",
                messageParams: [oResult.attempts, sErrorMessage],
                fallbackText: "Retry failed after " + oResult.attempts + " attempts: " + sErrorMessage,
                shouldSetBanner: true,
                bannerText: sErrorMessage
            };
        }

        if (oResult && oResult.reason === "error") {
            return {
                messageKey: "retryLoadFailed",
                messageParams: [sErrorMessage],
                fallbackText: "Retry failed: " + sErrorMessage,
                shouldSetBanner: true,
                bannerText: sErrorMessage
            };
        }

        return {
            messageKey: (mArgs && mArgs.unknownFallbackKey) || "loadErrorMessage",
            messageParams: null,
            fallbackText: "Unexpected retry result",
            shouldSetBanner: true,
            bannerText: "Unexpected retry result"
        };
    }

    function applyBannerState(mArgs) {
        var oStateModel = mArgs && mArgs.stateModel;
        if (!oStateModel || typeof oStateModel.setProperty !== "function") {
            return false;
        }

        var bShouldSetBanner = !!(mArgs && mArgs.shouldSetBanner);
        oStateModel.setProperty("/loadError", bShouldSetBanner);
        oStateModel.setProperty("/loadErrorMessage", bShouldSetBanner ? String((mArgs && mArgs.bannerText) || "") : "");
        return true;
    }

    function presentRetryOutcome(mArgs) {
        var oPresentation = resolveRetryOutcomePresentation(mArgs || {});
        var sText = getBundleText({
            bundle: mArgs && mArgs.bundle,
            key: oPresentation.messageKey,
            params: oPresentation.messageParams,
            fallbackText: oPresentation.fallbackText
        });

        applyBannerState({
            stateModel: mArgs && mArgs.stateModel,
            shouldSetBanner: oPresentation.shouldSetBanner,
            bannerText: oPresentation.bannerText
        });

        var fnShowToast = mArgs && mArgs.showToast;
        if (typeof fnShowToast === "function" && sText) {
            try {
                fnShowToast(sText);
                return {
                    ok: true,
                    reason: "presented",
                    message: sText,
                    presentation: oPresentation
                };
            } catch (_e) {
                return {
                    ok: false,
                    reason: "toast_error",
                    message: sText,
                    presentation: oPresentation
                };
            }
        }

        return {
            ok: false,
            reason: "missing_toast_adapter",
            message: sText,
            presentation: oPresentation
        };
    }

    return {
        getBundleText: getBundleText,
        resolveRetryOutcomePresentation: resolveRetryOutcomePresentation,
        applyBannerState: applyBannerState,
        presentRetryOutcome: presentRetryOutcome
    };
});
