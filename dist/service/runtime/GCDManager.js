sap.ui.define([
    "sap/ui/base/EventProvider",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/shared/ManagerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/shared/TimerRuntime"
], function (EventProvider, ManagerRuntime, TimerRuntime) {
    "use strict";

    return EventProvider.extend("PRODUCTION_CONTROL_CHECKLIST.service.runtime.GCDManager", {
        constructor: function (mOptions) {
            ManagerRuntime.initEventProvider(EventProvider, this, arguments);
            this._iIntervalMs = ManagerRuntime.readNumberOption(mOptions, "intervalMs");
            this._iTimer = null;
            this._bRunning = false;
        },

        start: function () {
            this._bRunning = true;
            this.resetOnFullSave();
        },

        resetOnFullSave: function () {
            if (!TimerRuntime.isValidDelay(this._iIntervalMs, 1000)) {
                return;
            }
            this._iTimer = TimerRuntime.restartTimeout(this._iTimer, function () {
                this.fireEvent("gcdExpired");
            }.bind(this), this._iIntervalMs);
        },


        setIntervalMs: function (iIntervalMs) {
            var iNext = Number(iIntervalMs);
            if (!TimerRuntime.isValidDelay(iNext, 1000)) {
                return;
            }
            this._iIntervalMs = iNext;
            if (this._bRunning) {
                this.resetOnFullSave();
            }
        },
        stop: function () {
            this._bRunning = false;
            this._iTimer = TimerRuntime.clearTimer(this._iTimer, clearTimeout);
        },
        destroyManager: function () {
            this.stop();
        },
        isRunning: function () {
            return this._bRunning;
        }
    });
});
