sap.ui.define([], function () {
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

    function normalizeODataError(oError) {
        var oEnvelope = pickEnvelope(oError);
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
        return {
            code: sCode,
            message: sMessage,
            statusCode: Number((oError && (oError.statusCode || oError.status)) || 0) || 0,
            details: extractDetails(oEnvelope),
            responseHeaders: (oError && oError.responseHeaders) || {}
        };
    }

    return {
        normalizeError: normalizeODataError,
        normalizeODataError: normalizeODataError
    };
});
