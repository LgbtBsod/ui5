sap.ui.define([
    "checklist/app/util/RuntimeTimerSanitizer",
    "checklist/app/util/runtime/TimerDefaults",
    "checklist/app/service/framework/ModelStateRuntime"
], function (RuntimeTimerSanitizer, TimerDefaults, ModelStateRuntime) {
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
        return ModelStateRuntime.readOnModel(oStateModel, "/timers/" + sTimerKey, undefined);
    }

    return {
        buildDefaultTimerMap: buildDefaultTimerMap,
        normalize: normalize,
        read: read
    };
});
