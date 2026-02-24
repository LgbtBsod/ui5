sap.ui.define([
  "sap/ui/base/EventProvider"
], function (EventProvider) {
  "use strict";

  return EventProvider.extend("sap_ui5.manager.AutoSaveCoordinator", {
    constructor: function (mOptions) {
      EventProvider.apply(this, arguments);
      this._iDebounceMs = (mOptions && mOptions.debounceMs) || 30 * 1000;
      this._iIntervalMs = (mOptions && mOptions.intervalMs) || 60 * 1000;
      this._fnShouldSave = (mOptions && mOptions.shouldSave) || function () { return false; };
      this._fnBuildPayload = (mOptions && mOptions.buildPayload) || function () { return null; };
      this._fnSave = (mOptions && mOptions.saveFn) || function () { return Promise.resolve(null); };
      this._iDebounceTimer = null;
      this._iIntervalTimer = null;
      this._bRunning = false;
    },

    start: function () {
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

    touch: function () {
      if (!this._bRunning) {
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
      if (!this._fnShouldSave()) {
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
