sap.ui.define([
    "sap/ui/base/EventProvider",
    "checklist/app/service/runtime/shared/ManagerRuntime",
    "checklist/app/service/runtime/shared/TimerRuntime"
], function (EventProvider, ManagerRuntime, TimerRuntime) {
    "use strict";

    return EventProvider.extend("checklist.app.service.runtime.LockStatusMonitor", {
        constructor: function (mOptions) {
            ManagerRuntime.initEventProvider(EventProvider, this, arguments);
            this._iIntervalMs = ManagerRuntime.readNumberOption(mOptions, "intervalMs");
            this._fnCheck = (mOptions && mOptions.checkFn) || function () { return Promise.resolve({}); };
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
                this._fnCheck().then(function (oResult) {
                    this.fireEvent("status", oResult || {});
                }.bind(this)).catch(function (oError) {
                    this.fireEvent("statusError", { error: oError });
                }.bind(this));
            }.bind(this), this._iIntervalMs);
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
        stop: function () {
            this._bRunning = false;
            this._iTimer = TimerRuntime.clearTimer(this._iTimer, clearInterval);
        }
    });
});
