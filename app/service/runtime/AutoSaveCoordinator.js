sap.ui.define([
  "sap/ui/base/EventProvider"
], function (EventProvider) {
  "use strict";

  return EventProvider.extend("checklist.app.service.runtime.AutoSaveCoordinator", {
    constructor: function (mOptions) {
      EventProvider.apply(this, arguments);
      this._iDebounceMs = Number(mOptions && mOptions.debounceMs);
      this._iIntervalMs = Number(mOptions && mOptions.intervalMs);
      this._fnShouldSave = (mOptions && mOptions.shouldSave) || function () { return false; };
      this._fnGuard = (mOptions && mOptions.guardFn) || function () { return true; };
      this._fnBuildPayload = (mOptions && mOptions.buildPayload) || function () { return null; };
      this._fnSave = (mOptions && mOptions.saveFn) || function () { return Promise.resolve(null); };
      this._fnLockGuard = (mOptions && mOptions.lockGuardFn) || function () { return false; };
      this._iDebounceTimer = null;
      this._iIntervalTimer = null;
      this._bRunning = false;
    },

    start: function () {
      if (!Number.isFinite(this._iIntervalMs) || this._iIntervalMs < 1000) {
        return;
      }
      this.stop();
      this._bRunning = true;
      this._iIntervalTimer = setInterval(function () {
        this._runIfNeeded();
      }.bind(this), this._iIntervalMs);
    },

    stop: function () {
      this._bRunning = false;
      if (this._iDebounceTimer) {
        clearTimeout(this._iDebounceTimer);
        this._iDebounceTimer = null;
      }
      if (this._iIntervalTimer) {
        clearInterval(this._iIntervalTimer);
        this._iIntervalTimer = null;
      }
    },


    setIntervals: function (mIntervals) {
      var iDebounce = Number(mIntervals && mIntervals.debounceMs);
      var iInterval = Number(mIntervals && mIntervals.intervalMs);
      if (Number.isFinite(iDebounce) && iDebounce >= 1000) {
        this._iDebounceMs = iDebounce;
      }
      if (Number.isFinite(iInterval) && iInterval >= 1000) {
        this._iIntervalMs = iInterval;
      }
      if (this._bRunning) {
        this.start();
      }
    },
    touch: function () {
      if (!this._bRunning) {
        return;
      }
      if (!Number.isFinite(this._iDebounceMs) || this._iDebounceMs < 100) {
        return;
      }
      if (this._iDebounceTimer) {
        clearTimeout(this._iDebounceTimer);
      }
      this._iDebounceTimer = setTimeout(function () {
        this._runIfNeeded();
      }.bind(this), this._iDebounceMs);
    },

    _runIfNeeded: function () {
      if (!this._fnLockGuard()) {
        if (typeof console !== "undefined" && console.warn) {
          console.warn("[AutoSaveCoordinator] autosave aborted: mode must be EDIT and lockOperationState must be LOCKED");
        }
        return Promise.resolve(null);
      }
      if (!this._fnGuard() || !this._fnShouldSave()) {
        return Promise.resolve(null);
      }
      var oPayload = this._fnBuildPayload();
      if (!oPayload) {
        return Promise.resolve(null);
      }
      this.fireEvent("autosaveStart", { payload: oPayload });
      return this._fnSave(oPayload).then(function (oResult) {
        this.fireEvent("autosaveDone", { result: oResult || null });
      }.bind(this)).catch(function (oError) {
        this.fireEvent("autosaveError", { error: oError });
      }.bind(this));
    }
  });
});
