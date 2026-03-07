sap.ui.define([
    "sap/ui/base/EventProvider",
    "checklist/app/manager/shared/ManagerRuntime",
    "checklist/app/manager/shared/TimerRuntime"
], function (EventProvider, ManagerRuntime, TimerRuntime) {
    "use strict";

    return EventProvider.extend("checklist.app.manager.LockStatusMonitor", {
        constructor: function (mOptions) {
            ManagerRuntime.initEventProvider(EventProvider, this, arguments);
            this._iIntervalMs = ManagerRuntime.readNumberOption(mOptions, "intervalMs");
            this._fnCheck = (mOptions && mOptions.checkFn) || function () { return Promise.resolve({}); };
            this._iTimer = null;
        },

        start: function () {
            if (!TimerRuntime.isValidDelay(this._iIntervalMs, 1000)) {
                return;
            }
            this.stop();
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
            if (this._iTimer) {
                this.start();
            }
        },
        stop: function () {
            this._iTimer = TimerRuntime.clearTimer(this._iTimer, clearInterval);
        }
    });
});
