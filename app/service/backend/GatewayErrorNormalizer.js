sap.ui.define([], function () {
    "use strict";

    function normalizeMethod(vMethod) {
        return String(vMethod || "").trim().toUpperCase();
    }

    function normalizeStatusCode(oError) {
        return Number((oError && (oError.statusCode || oError.status)) || 0) || 0;
    }

    function normalizeCode(oError) {
        return String((oError && oError.code) || "").trim().toUpperCase();
    }

    function normalizeMessage(oError) {
        return String((oError && oError.message) || "").trim().toUpperCase();
    }

    function normalizeDetails(oError) {
        return Array.isArray(oError && oError.details) ? oError.details : [];
    }

    function normalizeHeaders(oError) {
        var mHeaders = oError && oError.responseHeaders;
        return mHeaders && typeof mHeaders === "object" ? mHeaders : {};
    }

    function isCsrfError(oError) {
        var iStatusCode = normalizeStatusCode(oError);
        var sCode = normalizeCode(oError);
        var sMessage = normalizeMessage(oError);
        var aDetails = normalizeDetails(oError);
        var mHeaders = normalizeHeaders(oError);
        if (iStatusCode !== 403) {
            return false;
        }
        if (/CSRF|X-CSRF-TOKEN/.test(sCode) || /CSRF|X-CSRF-TOKEN/.test(sMessage)) {
            return true;
        }
        if (String(mHeaders["x-csrf-token"] || "").trim().toLowerCase() === "required") {
            return true;
        }
        return aDetails.some(function (oDetail) {
            return /CSRF|X-CSRF-TOKEN/.test(normalizeCode(oDetail)) || /CSRF|X-CSRF-TOKEN/.test(normalizeMessage(oDetail));
        });
    }

    function isTimeoutError(oError) {
        return normalizeCode(oError) === "REQUEST_TIMEOUT" || /TIMEOUT/.test(normalizeMessage(oError));
    }

    function isNetworkError(oError) {
        var sCode = normalizeCode(oError);
        var sMessage = normalizeMessage(oError);
        var iStatusCode = normalizeStatusCode(oError);
        if (isTimeoutError(oError)) {
            return true;
        }
        if (sCode === "OUTDATED_RESPONSE") {
            return false;
        }
        return iStatusCode === 0 || /NETWORK|OFFLINE|FAILED TO FETCH/.test(sMessage);
    }

    function classify(vMethod, oError) {
        var sMethod = normalizeMethod(vMethod);
        var iStatusCode = normalizeStatusCode(oError);
        var sCode = normalizeCode(oError);
        var bSafeRead = sMethod === "GET" || sMethod === "GET_FUNCTION";

        if (sCode === "OUTDATED_RESPONSE") {
            return {
                kind: "OUTDATED",
                retryable: false
            };
        }
        if (isNetworkError(oError)) {
            return {
                kind: isTimeoutError(oError) ? "TIMEOUT" : "NETWORK",
                retryable: bSafeRead
            };
        }
        if (iStatusCode === 401) {
            return { kind: "AUTH", retryable: false };
        }
        if (iStatusCode === 403) {
            if (isCsrfError(oError)) {
                return { kind: "CSRF", retryable: false };
            }
            return { kind: "PERMISSION", retryable: false };
        }
        if (iStatusCode === 404) {
            return { kind: "NOT_FOUND", retryable: false };
        }
        if (iStatusCode === 409) {
            return { kind: "CONFLICT", retryable: false };
        }
        if (iStatusCode >= 500) {
            return { kind: "SERVER", retryable: bSafeRead };
        }
        return {
            kind: "UNKNOWN",
            retryable: false
        };
    }

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

    function extractPrefixedCode(sMessage) {
        var oMatch = /^([A-Z][A-Z0-9_]+):\s*(.+)$/.exec(String(sMessage || "").trim());
        if (!oMatch) {
            return null;
        }
        return {
            code: String(oMatch[1] || "").trim(),
            message: String(oMatch[2] || "").trim()
        };
    }

    function extractTopLevelFields(oPayload) {
        var sCode = String(
            (oPayload && (oPayload.code || oPayload.Code || oPayload.reason_code || oPayload.ReasonCode)) ||
            ""
        ).trim();
        return {
            code: sCode,
            lockRefreshed: !!(oPayload && oPayload.lock_refreshed),
            lockExpiresAt: String((oPayload && (oPayload.lock_expires_at || oPayload.lock_expires)) || "").trim(),
            serverNow: String((oPayload && oPayload.server_now) || "").trim(),
            requestId: String((oPayload && (oPayload.request_id || oPayload.requestId)) || "").trim(),
            ownerSessionMatch: oPayload && Object.prototype.hasOwnProperty.call(oPayload, "owner_session_match")
                ? !!oPayload.owner_session_match
                : null
        };
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
        var oPayload = (oError && oError.responseJSON) || parseJsonSafe(oError && oError.responseText) || oError || {};
        var mHeaders = parseResponseHeaders((oError && oError.responseHeaders) || {});
        var oPrefixedMessage;
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
        oPrefixedMessage = extractPrefixedCode(sMessage);
        if (oPrefixedMessage && (!oPayload.code && !oPayload.reason_code) && (!oEnvelope || !oEnvelope.code)) {
            sMessage = oPrefixedMessage.message || sMessage;
        }
        var oNormalized = {
            code: sCode,
            message: sMessage,
            statusCode: Number((oError && (oError.statusCode || oError.status)) || 0) || 0,
            details: extractDetails(oEnvelope),
            responseHeaders: mHeaders,
            backend: (function () {
                var oBackend = extractTopLevelFields(oPayload);
                if (!oBackend.code && oPrefixedMessage) {
                    oBackend.code = oPrefixedMessage.code;
                }
                return oBackend;
            }()),
            correlationId: String(
                mHeaders["x-correlation-id"]
                || mHeaders["x-request-id"]
                || extractTopLevelFields(oPayload).requestId
                || (oError && (oError.correlationId || oError.requestId))
                || ""
            ).trim()
        };
        var oPolicy = classify((oError && oError.requestMethod) || "", oNormalized);
        oNormalized.kind = oPolicy.kind;
        oNormalized.retryable = !!oPolicy.retryable;
        return oNormalized;
    }

    return {
        normalizeError: normalizeODataError,
        normalizeODataError: normalizeODataError
    };
});
