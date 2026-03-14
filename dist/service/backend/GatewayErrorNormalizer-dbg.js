sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/RequestResiliencePolicy"
], function (RequestResiliencePolicy) {
    "use strict";

    function parseJsonSafe(vRaw) {
        if (!vRaw || typeof vRaw !== "string") {
            return null;
        }
        try {
            return JSON.parse(vRaw);
        } catch (_e) {
            return null;
        }
    }

    function asArray(vValue) {
        return Array.isArray(vValue) ? vValue : [];
    }

    function pickEnvelope(oError) {
        var oPayload = (oError && oError.responseJSON) || parseJsonSafe(oError && oError.responseText) || oError;
        return oPayload && oPayload.error ? oPayload.error : null;
    }

    function extractDetails(oEnvelope) {
        var oInner = oEnvelope && oEnvelope.innererror;
        return asArray(oInner && oInner.errordetails).map(function (oDetail) {
            return {
                code: String((oDetail && (oDetail.code || oDetail.Code)) || ""),
                message: String((oDetail && (oDetail.message || oDetail.Message)) || "")
            };
        }).filter(function (oDetail) {
            return !!(oDetail.code || oDetail.message);
        });
    }

    function parseResponseHeaders(vHeaders) {
        if (!vHeaders) { return {}; }
        if (typeof vHeaders === "object" && !Array.isArray(vHeaders)) { return vHeaders; }
        var mResult = {};
        String(vHeaders).split(/\r?\n/).forEach(function (sLine) {
            var iColon = sLine.indexOf(":");
            if (iColon < 1) { return; }
            var sKey = sLine.slice(0, iColon).trim().toLowerCase();
            var sVal = sLine.slice(iColon + 1).trim();
            if (sKey) { mResult[sKey] = sVal; }
        });
        return mResult;
    }

    function normalizeODataError(oError) {
        var oEnvelope = pickEnvelope(oError);
        var mHeaders = parseResponseHeaders((oError && oError.responseHeaders) || {});
        var sCode = String(
            (oEnvelope && oEnvelope.code) ||
            (oError && (oError.code || oError.statusCode || oError.status)) ||
            "SYSTEM_ERROR"
        );
        var sMessage = String(
            (oEnvelope && oEnvelope.message && (oEnvelope.message.value || oEnvelope.message)) ||
            (oError && oError.message) ||
            "OData request failed"
        );
        var oNormalized = {
            code: sCode,
            message: sMessage,
            statusCode: Number((oError && (oError.statusCode || oError.status)) || 0) || 0,
            details: extractDetails(oEnvelope),
            responseHeaders: mHeaders,
            correlationId: String(
                mHeaders["x-correlation-id"]
                || mHeaders["x-request-id"]
                || (oError && (oError.correlationId || oError.requestId))
                || ""
            ).trim()
        };
        var oPolicy = RequestResiliencePolicy.classify((oError && oError.requestMethod) || "", oNormalized);
        oNormalized.kind = oPolicy.kind;
        oNormalized.retryable = !!oPolicy.retryable;
        return oNormalized;
    }

    return {
        normalizeError: normalizeODataError,
        normalizeODataError: normalizeODataError
    };
});
