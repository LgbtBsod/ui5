sap.ui.define([
    "sap/ui/base/EventProvider"
], function (EventProvider) {
    "use strict";

    return EventProvider.extend("sap_ui5.manager.GCDManager", {
        constructor: function (mOptions) {
            EventProvider.apply(this, arguments);
            this._iIntervalMs = (mOptions && mOptions.intervalMs) || 5 * 60 * 1000;
            this._iTimer = null;
            this.resetOnFullSave();
        },

        resetOnFullSave: function () {
            if (this._iTimer) {
                clearTimeout(this._iTimer);
            }

            this._iTimer = setTimeout(function () {
                this.fireEvent("gcdExpired");
            }.bind(this), this._iIntervalMs);
        },

        destroyManager: function () {
            if (this._iTimer) {
                clearTimeout(this._iTimer);
                this._iTimer = null;
            }
        }
    });
});
