sap.ui.define([], function () {
    "use strict";

    var DEFAULT_SAFE_TIMEOUT_MS = 15000;
    var DEFAULT_MUTATE_TIMEOUT_MS = 45000;
    var DEFAULT_SAFE_RETRY_COUNT = 1;
    var DEFAULT_RETRY_BASE_DELAY_MS = 400;
    var DEFAULT_RETRY_MAX_DELAY_MS = 2500;

    function normalizeMethod(vMethod) {
        return String(vMethod || "GET").trim().toUpperCase() || "GET";
    }

    function isSafeRead(vMethod) {
        var sMethod = normalizeMethod(vMethod);
        return sMethod === "GET" || sMethod === "GET_FUNCTION";
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
        var bSafeRead = isSafeRead(sMethod);

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

    function resolveTimeoutMs(vMethod, iTimeoutMs) {
        var iConfigured = Number(iTimeoutMs || 0);
        if (Number.isFinite(iConfigured) && iConfigured >= 1000) {
            return iConfigured;
        }
        return isSafeRead(vMethod) ? DEFAULT_SAFE_TIMEOUT_MS : DEFAULT_MUTATE_TIMEOUT_MS;
    }

    function resolveRetryCount(vMethod, iRetryCount) {
        var iConfigured = Number(iRetryCount);
        if (Number.isFinite(iConfigured) && iConfigured >= 0) {
            return iConfigured;
        }
        return isSafeRead(vMethod) ? DEFAULT_SAFE_RETRY_COUNT : 0;
    }

    function resolveRetryDelayMs(vMethod, iAttemptIndex, iBaseDelayMs, iMaxDelayMs) {
        var iBase = Number(iBaseDelayMs);
        var iMax = Number(iMaxDelayMs);
        var iAttempt = Math.max(0, Number(iAttemptIndex) || 0);
        if (!isSafeRead(vMethod)) {
            return 0;
        }
        if (!Number.isFinite(iBase) || iBase < 50) {
            iBase = DEFAULT_RETRY_BASE_DELAY_MS;
        }
        if (!Number.isFinite(iMax) || iMax < iBase) {
            iMax = DEFAULT_RETRY_MAX_DELAY_MS;
        }
        return Math.min(iMax, iBase * Math.pow(2, iAttempt));
    }

    function matrix() {
        return {
            safeRead: {
                timeout: "retry_once",
                offline: "retry_once",
                status401: "fail_auth",
                status403: "fail_permission",
                status404: "fail_not_found",
                status409: "fail_conflict",
                status5xx: "retry_once"
            },
            mutate: {
                timeout: "fail_no_retry",
                offline: "fail_no_retry",
                status401: "fail_auth",
                status403: "fail_permission",
                status404: "fail_not_found",
                status409: "fail_conflict",
                status5xx: "fail_no_retry"
            }
        };
    }

    return {
        classify: classify,
        isNetworkError: isNetworkError,
        isSafeRead: isSafeRead,
        isTimeoutError: isTimeoutError,
        matrix: matrix,
        resolveRetryDelayMs: resolveRetryDelayMs,
        resolveRetryCount: resolveRetryCount,
        resolveTimeoutMs: resolveTimeoutMs
    };
});
