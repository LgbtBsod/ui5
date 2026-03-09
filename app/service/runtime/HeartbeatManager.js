sap.ui.define([
    "sap/ui/base/EventProvider",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/shared/ManagerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/shared/TimerRuntime"
], function (EventProvider, ManagerRuntime, TimerRuntime) {
    "use strict";

    return EventProvider.extend("PRODUCTION_CONTROL_CHECKLIST.service.runtime.HeartbeatManager", {
        constructor: function (mOptions) {
            ManagerRuntime.initEventProvider(EventProvider, this, arguments);
            this._iIntervalMs = ManagerRuntime.readNumberOption(mOptions, "intervalMs");
            this._fnHeartbeat = (mOptions && mOptions.heartbeatFn) || function () { return Promise.resolve({}); };
            this._iTimer = null;
            this._bRunning = false;
        },

        start: function () {
            if (!TimerRuntime.isValidDelay(this._iIntervalMs, 1000)) {
                return;
            }
            this.stop();
            this._bRunning = true;
            this._iTimer = TimerRuntime.restartInterval(this._iTimer, function () {
                this._fnHeartbeat().then(function (oResult) {
                    this.fireEvent("heartbeat", oResult || {});
                }.bind(this)).catch(function (oError) {
                    this.fireEvent("heartbeatError", { error: oError });
                }.bind(this));
            }.bind(this), this._iIntervalMs);
        },

        stop: function () {
            this._bRunning = false;
            this._iTimer = TimerRuntime.clearTimer(this._iTimer, clearInterval);
        },

        setIntervalMs: function (iIntervalMs) {
            var iNext = Number(iIntervalMs);
            if (!TimerRuntime.isValidDelay(iNext, 1000)) {
                return;
            }
            this._iIntervalMs = iNext;
            if (this._bRunning) {
                this.start();
            }
        },

        isRunning: function () {
            return this._bRunning;
        }
    });
});
