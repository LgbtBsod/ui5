sap.ui.define([
  "sap/ui/base/EventProvider"
], function (EventProvider) {
  "use strict";

  return EventProvider.extend("sap_ui5.manager.ConnectivityCoordinator", {
    constructor: function (mOptions) {
      EventProvider.apply(this, arguments);
      this._iGraceMs = (mOptions && mOptions.graceMs) || 60 * 1000;
      this._iGraceTimer = null;
      this._fnOnline = this._onOnline.bind(this);
      this._fnOffline = this._onOffline.bind(this);
    },

    start: function () {
      window.addEventListener("online", this._fnOnline);
      window.addEventListener("offline", this._fnOffline);
      this.fireEvent("state", { online: navigator.onLine !== false, isGrace: false, graceExpiresAt: null });
    },

    stop: function () {
      window.removeEventListener("online", this._fnOnline);
      window.removeEventListener("offline", this._fnOffline);
      if (this._iGraceTimer) {
        clearTimeout(this._iGraceTimer);
        this._iGraceTimer = null;
      }
    },


    setGraceMs: function (iGraceMs) {
      var iNext = Number(iGraceMs);
      if (!Number.isFinite(iNext) || iNext < 1000) {
        return;
      }
      this._iGraceMs = iNext;
    },
    _onOffline: function () {
      var sGraceUntil = new Date(Date.now() + this._iGraceMs).toISOString();
      this.fireEvent("state", { online: false, isGrace: true, graceExpiresAt: sGraceUntil });
      if (this._iGraceTimer) {
        clearTimeout(this._iGraceTimer);
      }
      this._iGraceTimer = setTimeout(function () {
        this.fireEvent("graceExpired", { online: false });
      }.bind(this), this._iGraceMs);
    },

    _onOnline: function () {
      if (this._iGraceTimer) {
        clearTimeout(this._iGraceTimer);
        this._iGraceTimer = null;
      }
      this.fireEvent("state", { online: true, isGrace: false, graceExpiresAt: null });
    }
  });
});
