sap.ui.define([
    "checklist/app/util/runtime/TimerDefaults",
    "checklist/app/util/runtime/TimerSanitizer"
], function (RANGES, TimerSanitizer) {
    "use strict";

    function pickValue(oRuntime, oDefaults, mSpec) {
        var aMsKeys = mSpec.msKeys || [];
        var aSecKeys = mSpec.secKeys || [];
        var i;
        for (i = 0; i < aMsKeys.length; i += 1) {
            if (oRuntime[aMsKeys[i]] !== undefined && oRuntime[aMsKeys[i]] !== null && oRuntime[aMsKeys[i]] !== "") { return { value: oRuntime[aMsKeys[i]], mode: "ms" }; }
        }
        for (i = 0; i < aSecKeys.length; i += 1) {
            if (oRuntime[aSecKeys[i]] !== undefined && oRuntime[aSecKeys[i]] !== null && oRuntime[aSecKeys[i]] !== "") { return { value: oRuntime[aSecKeys[i]], mode: "sec" }; }
        }
        return { value: oDefaults[mSpec.key] !== undefined ? oDefaults[mSpec.key] : mSpec.defaultValue, mode: "ms" };
    }

    function sanitizeTimers(oRuntimeSettingsPayload, oSafeDefaults) {
        var oRuntime = oRuntimeSettingsPayload || {};
        var oDefaults = oSafeDefaults || {};
        var oTimers = {};
        Object.keys(RANGES).forEach(function (sTimerKey) {
            var mSpec = Object.assign({ key: sTimerKey }, RANGES[sTimerKey]);
            var oPicked = pickValue(oRuntime, oDefaults, mSpec);
            oTimers[sTimerKey] = oPicked.mode === "sec" ? TimerSanitizer.sanitizeSecToMs(oPicked.value, mSpec) : TimerSanitizer.sanitizeMs(oPicked.value, mSpec);
        });
        return oTimers;
    }

    return {
        RANGES: RANGES,
        parseNumber: TimerSanitizer.parseNumber,
        clamp: TimerSanitizer.clamp,
        sanitizeMs: TimerSanitizer.sanitizeMs,
        sanitizeSecToMs: TimerSanitizer.sanitizeSecToMs,
        sanitizeTimers: sanitizeTimers
    };
});
