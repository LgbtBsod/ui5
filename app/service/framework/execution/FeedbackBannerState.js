sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimeInput",
    "PRODUCTION_CONTROL_CHECKLIST/constants/FeedbackConstants"
], function (RuntimeInput, FeedbackConstants) {
    "use strict";

    function sanitizeErrorDetail(sDetail) {
        return RuntimeInput.asString(sDetail)
            .replace(/(authorization|token|password)\s*[:=]\s*[^,\s]+/ig, "$1=***")
            .trim();
    }

    function normalizeSeverity(sSeverity) {
        var sValue = RuntimeInput.asString(sSeverity, FeedbackConstants.SEVERITY.INFO).toLowerCase();
        return Object.keys(FeedbackConstants.SEVERITY).some(function (sKey) {
            return FeedbackConstants.SEVERITY[sKey] === sValue;
        }) ? sValue : FeedbackConstants.SEVERITY.INFO;
    }

    function normalizeScope(sScope) {
        var sValue = RuntimeInput.asString(sScope, FeedbackConstants.SCOPE.GLOBAL).toLowerCase();
        return sValue === FeedbackConstants.SCOPE.ROUTE ? FeedbackConstants.SCOPE.ROUTE : FeedbackConstants.SCOPE.GLOBAL;
    }

    function toUi5MessageType(sSeverity) {
        return FeedbackConstants.MESSAGE_TYPE_BY_SEVERITY[normalizeSeverity(sSeverity)] || FeedbackConstants.MESSAGE_TYPE_BY_SEVERITY[FeedbackConstants.SEVERITY.INFO];
    }

    function create(mInput, mOptions) {
        var oInput = RuntimeInput.asObject(mInput);
        var oOptions = RuntimeInput.asObject(mOptions);
        var fnResolveText = typeof oOptions.resolveText === "function"
            ? oOptions.resolveText
            : function (sKey, aArgs) { return RuntimeInput.asString(sKey); };
        var sText = RuntimeInput.asString(oInput.text || "").trim();
        var sTextKey = RuntimeInput.asString(oInput.textKey || "").trim();
        var sCorrelationId = RuntimeInput.asString(oInput.correlationId || "").trim();
        if (!sText && sTextKey) {
            sText = RuntimeInput.asString(fnResolveText(sTextKey, oInput.textArgs || []), sTextKey);
        }
        return {
            visible: oInput.visible !== false,
            severity: normalizeSeverity(oInput.severity),
            scope: normalizeScope(oInput.scope),
            text: sText,
            details: sanitizeErrorDetail(oInput.details || ""),
            correlationId: sCorrelationId,
            retryAction: RuntimeInput.asString(oInput.retryAction || "").trim(),
            retryTextKey: RuntimeInput.asString(oInput.retryTextKey || "").trim()
        };
    }

    function empty() {
        return create({ visible: false, severity: FeedbackConstants.SEVERITY.INFO });
    }

    return {
        create: create,
        empty: empty,
        normalizeScope: normalizeScope,
        normalizeSeverity: normalizeSeverity,
        toUi5MessageType: toUi5MessageType
    };
});
