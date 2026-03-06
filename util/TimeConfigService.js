sap.ui.define([
    "sap_ui5/util/RuntimeTimerSanitizer",
    "sap_ui5/util/runtime/TimerDefaults"
], function (RuntimeTimerSanitizer, TimerDefaults) {
    "use strict";

    function buildDefaultTimerMap() {
        var oDefaults = {};
        Object.keys(TimerDefaults || {}).forEach(function (sKey) {
            var mSpec = TimerDefaults[sKey] || {};
            oDefaults[sKey] = mSpec.defaultValue;
        });
        return oDefaults;
    }

    function normalize(oRuntimePayload, oCurrentTimers) {
        return RuntimeTimerSanitizer.sanitizeTimers(
            oRuntimePayload || {},
            oCurrentTimers || buildDefaultTimerMap()
        );
    }

    function read(oStateModel, sTimerKey) {
        if (!oStateModel || !sTimerKey) {
            return undefined;
        }
        return oStateModel.getProperty("/timers/" + sTimerKey);
    }

    return {
        buildDefaultTimerMap: buildDefaultTimerMap,
        normalize: normalize,
        read: read
    };
});
