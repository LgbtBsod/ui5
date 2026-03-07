sap.ui.define([
    "sap/ui/base/EventProvider",
    "checklist/app/manager/shared/ManagerRuntime",
    "checklist/app/manager/shared/TimerRuntime"
], function (EventProvider, ManagerRuntime, TimerRuntime) {
    "use strict";

    return EventProvider.extend("checklist.app.manager.GCDManager", {
        constructor: function (mOptions) {
            ManagerRuntime.initEventProvider(EventProvider, this, arguments);
            this._iIntervalMs = ManagerRuntime.readNumberOption(mOptions, "intervalMs");
            this._iTimer = null;
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
            this.resetOnFullSave();
        },
        destroyManager: function () {
            this._iTimer = TimerRuntime.clearTimer(this._iTimer, clearTimeout);
        }
    });
});
