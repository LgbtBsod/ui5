sap.ui.define([
    "sap_ui5/service/framework/FeedbackBannerRuntime"
], function (FeedbackBannerRuntime) {
    "use strict";

    function resolveCorrelationId(oError, FeedbackPolicy) {
        var oNormalizedError = FeedbackPolicy && FeedbackPolicy.normalize ? FeedbackPolicy.normalize(oError || {}) : null;
        var oParams = oNormalizedError && oNormalizedError.params;
        return String(
            (oParams && (oParams.correlationId || oParams.correlation_id || oParams.requestId || oParams.request_id)) ||
            (oError && (oError.correlationId || oError.correlation_id || oError.requestId || oError.request_id)) ||
            ""
        ).trim();
    }

    function isSessionExpiredError(oError) {
        var iStatus = Number((oError && (oError.statusCode || oError.status)) || 0);
        var sCode = String((oError && oError.code) || "").toUpperCase();
        var sMessage = String((oError && oError.message) || "").toUpperCase();
        if (iStatus === 401 || iStatus === 403) {
            return true;
        }
        return sCode === "SESSION_UNAVAILABLE" || sCode === "AUTH_REQUIRED" || /SESSION|AUTH|CSRF/.test(sMessage);
    }

    function create(mOptions) {
        var oOptions = mOptions || {};
        var oStateModel = oOptions.stateModel;
        var FeedbackPolicy = oOptions.feedbackPolicy;
        var fnBundleText = oOptions.bundleText || function (sKey) { return sKey; };

        function setGlobalBanner(mBannerInput) {
            var mInput = mBannerInput || {};
            FeedbackBannerRuntime.setBanner(oStateModel, "global", mInput, {
                resolveText: fnBundleText
            });
        }

        function clearGlobalBanner() {
            FeedbackBannerRuntime.clearBanner(oStateModel, "global");
        }

        return {
            resolveCorrelationId: function (oError) {
                return resolveCorrelationId(oError, FeedbackPolicy);
            },
            isSessionExpiredError: isSessionExpiredError,
            setGlobalBanner: setGlobalBanner,
            clearGlobalBanner: clearGlobalBanner
        };
    }

    return {
        create: create
    };
});
