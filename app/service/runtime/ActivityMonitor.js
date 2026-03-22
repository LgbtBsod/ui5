sap.ui.define([
    "sap/ui/base/EventProvider",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/shared/TimerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/RuntimeEventContracts"
], function (EventProvider, TimerRuntime, RuntimeEventContracts) {
    "use strict";

    return EventProvider.extend("PRODUCTION_CONTROL_CHECKLIST.service.runtime.ActivityMonitor", {
        constructor: function (mOptions) {
            EventProvider.apply(this, arguments);
            this._iIdleMs = Number(mOptions && mOptions.idleMs);
            this._iTimer = null;
            this._iThrottleMs = Math.max(250, Number(mOptions && mOptions.throttleMs) || 750);
            this._iLastActivityAt = 0;
            this._fnActivityHandler = this._reset.bind(this);
            this._aEvents = ["click", "keydown", "input", "change", "scroll", "touchstart"];
        },

        start: function () {
            if (!Number.isFinite(this._iIdleMs) || this._iIdleMs < 1000) {
                return;
            }
            this.stop();
            this._aEvents.forEach(function (sEvt) {
                window.addEventListener(sEvt, this._fnActivityHandler, { passive: sEvt === "scroll" || sEvt === "touchstart" });
            }.bind(this));
            this._reset();
        },

        _reset: function () {
            var iNow = Date.now();
            if (this._iLastActivityAt && iNow - this._iLastActivityAt < this._iThrottleMs) {
                return;
            }
            this._iLastActivityAt = iNow;
            this.fireEvent(RuntimeEventContracts.ACTIVITY, { at: new Date().toISOString() });
            this._iTimer = TimerRuntime.restartTimeout(this._iTimer, function () {
                this.fireEvent(RuntimeEventContracts.IDLE_TIMEOUT);
            }.bind(this), this._iIdleMs);
        },


        setIdleMs: function (iIdleMs) {
            var iNext = Number(iIdleMs);
            if (!Number.isFinite(iNext) || iNext < 1000) {
                return;
            }
            this._iIdleMs = iNext;
            if (this._iTimer) {
                this._reset();
            }
        },
        stop: function () {
            this._iTimer = TimerRuntime.clearTimer(this._iTimer, clearTimeout);
            this._iLastActivityAt = 0;

            this._aEvents.forEach(function (sEvt) {
                window.removeEventListener(sEvt, this._fnActivityHandler);
            }.bind(this));
        }
    });
});
