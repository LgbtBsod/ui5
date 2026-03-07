sap.ui.define([
    "sap_ui5/service/framework/RuntimeInput"
], function (RuntimeInput) {
    "use strict";

    function sanitizeErrorDetail(sDetail) {
        return RuntimeInput.asString(sDetail)
            .replace(/(authorization|token|password)\s*[:=]\s*[^,\s]+/ig, "$1=***")
            .trim();
    }

    function normalizeSeverity(sSeverity) {
        var sValue = RuntimeInput.asString(sSeverity, "info").toLowerCase();
        return ["info", "success", "warning", "error"].indexOf(sValue) >= 0 ? sValue : "info";
    }

    function toUi5MessageType(sSeverity) {
        var mMap = {
            success: "Success",
            warning: "Warning",
            error: "Error",
            info: "Information"
        };
        return mMap[normalizeSeverity(sSeverity)] || "Information";
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
            text: sText,
            details: sanitizeErrorDetail(oInput.details || ""),
            correlationId: sCorrelationId,
            retryAction: RuntimeInput.asString(oInput.retryAction || "").trim(),
            retryTextKey: RuntimeInput.asString(oInput.retryTextKey || "").trim()
        };
    }

    function empty() {
        return create({ visible: false, severity: "info" });
    }

    return {
        create: create,
        empty: empty,
        normalizeSeverity: normalizeSeverity,
        toUi5MessageType: toUi5MessageType
    };
});
