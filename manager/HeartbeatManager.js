sap.ui.define([
    "sap/ui/base/EventProvider"
], function (EventProvider) {
    "use strict";

    return EventProvider.extend("sap_ui5.manager.HeartbeatManager", {
        constructor: function (mOptions) {
            EventProvider.apply(this, arguments);
            this._iIntervalMs = (mOptions && mOptions.intervalMs) || 4 * 60 * 1000;
            this._fnHeartbeat = (mOptions && mOptions.heartbeatFn) || function () { return Promise.resolve({}); };
            this._iTimer = null;
        },

        start: function () {
            this.stop();
            this._iTimer = setInterval(function () {
                this._fnHeartbeat().then(function (oResult) {
                    this.fireEvent("heartbeat", oResult || {});
                }.bind(this)).catch(function (oError) {
                    this.fireEvent("heartbeatError", { error: oError });
                }.bind(this));
            }.bind(this), this._iIntervalMs);
        },

        stop: function () {
            if (this._iTimer) {
                clearInterval(this._iTimer);
                this._iTimer = null;
            }
        }
    });
});
