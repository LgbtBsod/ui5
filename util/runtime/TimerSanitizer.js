sap.ui.define(["sap_ui5/util/DebugLogger"], function (DebugLogger) {
    "use strict";

    function parseNumber(vValue) {
        var iParsed = Number(vValue);
        return Number.isFinite(iParsed) ? iParsed : NaN;
    }

    function clamp(iValue, iMin, iMax) {
        return Math.max(iMin, Math.min(iMax, iValue));
    }

    function warn(sKey, sReason, vValue, iFallback) {
        if (!DebugLogger.isEnabled()) { return; }
        DebugLogger.info("runtime", "timer_sanitized", { key: sKey, reason: sReason, value: vValue, fallback: iFallback });
    }

    function sanitizeMs(vValue, mSpec) {
        var iParsed = parseNumber(vValue);
        if (!Number.isFinite(iParsed)) { warn(mSpec.key, "missing_or_nan", vValue, mSpec.defaultValue); return mSpec.defaultValue; }
        if (!mSpec.allowZero && iParsed === 0) { warn(mSpec.key, "zero_not_allowed", vValue, mSpec.defaultValue); return mSpec.defaultValue; }
        if (iParsed < mSpec.min || iParsed > mSpec.max) { warn(mSpec.key, "clamped", vValue, mSpec.defaultValue); }
        return clamp(iParsed, mSpec.min, mSpec.max);
    }

    function sanitizeSecToMs(vValueSec, mSpec) {
        var iSec = parseNumber(vValueSec);
        return Number.isFinite(iSec) ? sanitizeMs(iSec * 1000, mSpec) : sanitizeMs(vValueSec, mSpec);
    }

    return {
        parseNumber: parseNumber,
        clamp: clamp,
        sanitizeMs: sanitizeMs,
        sanitizeSecToMs: sanitizeSecToMs
    };
});
