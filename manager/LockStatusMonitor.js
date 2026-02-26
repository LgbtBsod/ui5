sap.ui.define([
    "sap/ui/base/EventProvider"
], function (EventProvider) {
    "use strict";

    return EventProvider.extend("sap_ui5.manager.LockStatusMonitor", {
        constructor: function (mOptions) {
            EventProvider.apply(this, arguments);
            this._iIntervalMs = (mOptions && mOptions.intervalMs) || 60 * 1000;
            this._fnCheck = (mOptions && mOptions.checkFn) || function () { return Promise.resolve({}); };
            this._iTimer = null;
        },

        start: function () {
            this.stop();
            this._iTimer = setInterval(function () {
                this._fnCheck().then(function (oResult) {
                    this.fireEvent("status", oResult || {});
                }.bind(this)).catch(function (oError) {
                    this.fireEvent("statusError", { error: oError });
                }.bind(this));
            }.bind(this), this._iIntervalMs);
        },


        setIntervalMs: function (iIntervalMs) {
            var iNext = Number(iIntervalMs);
            if (!Number.isFinite(iNext) || iNext < 1000) {
                return;
            }
            this._iIntervalMs = iNext;
            if (this._iTimer) {
                this.start();
            }
        },
        stop: function () {
            if (this._iTimer) {
                clearInterval(this._iTimer);
                this._iTimer = null;
            }
        }
    });
});
