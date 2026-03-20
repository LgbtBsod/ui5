sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UiBehaviorPolicy"
], function (UiBehaviorPolicy) {
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

    function createFeedbackRuntime(oOptions) {
        var oUiBehavior = UiBehaviorPolicy.create({
            stateModel: oOptions.stateModel,
            resolveText: oOptions.bundleText || function (sKey) { return sKey; }
        });

        return {
            resolveCorrelationId: function (oError) {
                return resolveCorrelationId(oError, oOptions.feedbackPolicy);
            },
            isSessionExpiredError: isSessionExpiredError,
            setGlobalBanner: oUiBehavior.setGlobalBanner,
            clearGlobalBanner: oUiBehavior.clearGlobalBanner
        };
    }

    return {
        createFeedbackRuntime: createFeedbackRuntime
    };
});
