sap.ui.define([
  "sap/ui/base/EventProvider",
  "PRODUCTION_CONTROL_CHECKLIST/service/runtime/shared/TimerRuntime"
], function (EventProvider, TimerRuntime) {
  "use strict";

  return EventProvider.extend("PRODUCTION_CONTROL_CHECKLIST.service.runtime.ConnectivityCoordinator", {
    constructor: function (mOptions) {
      EventProvider.apply(this, arguments);
      this._iGraceMs = Number(mOptions && mOptions.graceMs);
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
      this._iGraceTimer = TimerRuntime.clearTimer(this._iGraceTimer, clearTimeout);
    },


    setGraceMs: function (iGraceMs) {
      var iNext = Number(iGraceMs);
      if (!Number.isFinite(iNext) || iNext < 1000) {
        return;
      }
      this._iGraceMs = iNext;
    },
    _onOffline: function () {
      if (!Number.isFinite(this._iGraceMs) || this._iGraceMs < 1000) {
        this.fireEvent("graceExpired", { online: false });
        return;
      }
      var sGraceUntil = new Date(Date.now() + this._iGraceMs).toISOString();
      this.fireEvent("state", { online: false, isGrace: true, graceExpiresAt: sGraceUntil });
      this._iGraceTimer = TimerRuntime.restartTimeout(this._iGraceTimer, function () {
        this.fireEvent("graceExpired", { online: false });
      }.bind(this), this._iGraceMs);
    },

    _onOnline: function () {
      this._iGraceTimer = TimerRuntime.clearTimer(this._iGraceTimer, clearTimeout);
      this.fireEvent("state", { online: true, isGrace: false, graceExpiresAt: null });
    }
  });
});
