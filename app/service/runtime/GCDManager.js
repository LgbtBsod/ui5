sap.ui.define([
    "sap/ui/base/EventProvider",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/RuntimeEventContracts"
], function (EventProvider, SchedulingRuntime, RuntimeEventContracts) {
    "use strict";

    return EventProvider.extend("PRODUCTION_CONTROL_CHECKLIST.service.runtime.GCDManager", {
        constructor: function (mOptions) {
            EventProvider.apply(this, arguments);
            this._iIntervalMs = Number(mOptions && mOptions.intervalMs);
            this._iTimer = null;
            this._bRunning = false;
        },

        start: function () {
            this._bRunning = true;
            this.resetOnFullSave();
        },

        resetOnFullSave: function () {
            if (!SchedulingRuntime.isValidDelay(this._iIntervalMs, 1000)) {
                return;
            }
            this._iTimer = SchedulingRuntime.restartTimer(this._iTimer, function () {
                this.fireEvent(RuntimeEventContracts.GCD_EXPIRED);
            }.bind(this), this._iIntervalMs);
        },


        setIntervalMs: function (iIntervalMs) {
            var iNext = Number(iIntervalMs);
            if (!SchedulingRuntime.isValidDelay(iNext, 1000)) {
                return;
            }
            this._iIntervalMs = iNext;
            if (this._bRunning) {
                this.resetOnFullSave();
            }
        },
        stop: function () {
            this._bRunning = false;
            this._iTimer = SchedulingRuntime.clearTimer(this._iTimer, clearTimeout);
        },
        isRunning: function () {
            return this._bRunning;
        }
    });
});
