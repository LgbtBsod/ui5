sap.ui.define([
    "sap/ui/base/EventProvider"
], function (EventProvider) {
    "use strict";

    return EventProvider.extend("sap_ui5.manager.ActivityMonitor", {
        constructor: function (mOptions) {
            EventProvider.apply(this, arguments);
            this._iIdleMs = (mOptions && mOptions.idleMs) || 10 * 60 * 1000;
            this._iTimer = null;
            this._fnActivityHandler = this._reset.bind(this);
            this._aEvents = ["click", "keydown", "mousemove", "touchstart"];
        },

        start: function () {
            this.stop();
            this._aEvents.forEach(function (sEvt) {
                window.addEventListener(sEvt, this._fnActivityHandler, { passive: true });
            }.bind(this));
            this._reset();
        },

        _reset: function () {
            this.fireEvent("activity", { at: new Date().toISOString() });
            if (this._iTimer) {
                clearTimeout(this._iTimer);
            }
            this._iTimer = setTimeout(function () {
                this.fireEvent("idleTimeout");
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
            if (this._iTimer) {
                clearTimeout(this._iTimer);
                this._iTimer = null;
            }

            this._aEvents.forEach(function (sEvt) {
                window.removeEventListener(sEvt, this._fnActivityHandler);
            }.bind(this));
        }
    });
});
