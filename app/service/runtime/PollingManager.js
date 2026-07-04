sap.ui.define([
    "sap/ui/base/EventProvider",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime"
], function (EventProvider, SchedulingRuntime) {
    "use strict";

    function normalizeInterval(mOptions) {
        return Number(mOptions && mOptions.intervalMs);
    }

    function normalizeHandler(mOptions, sKey) {
        return (mOptions && mOptions[sKey]) || function () {
            return Promise.resolve({});
        };
    }

    function normalizeEventName(mOptions, sKey, sFallback) {
        var sName = String((mOptions && mOptions[sKey]) || "").trim();
        return sName || sFallback;
    }

    function buildClass(sClassName, mDefaults) {
        var mConfig = Object.assign({
            runFnKey: "runFn",
            eventName: "tick",
            errorEventName: "tickError"
        }, mDefaults || {});

        return EventProvider.extend(sClassName, {
            constructor: function (mOptions) {
                EventProvider.apply(this, arguments);
                this._iIntervalMs = normalizeInterval(mOptions);
                this._fnRun = normalizeHandler(mOptions, mConfig.runFnKey);
                this._sEventName = normalizeEventName(mOptions, "eventName", mConfig.eventName);
                this._sErrorEventName = normalizeEventName(mOptions, "errorEventName", mConfig.errorEventName);
                this._iTimer = null;
                this._bRunning = false;
            },

            start: function () {
                if (!SchedulingRuntime.isValidDelay(this._iIntervalMs, 1000)) {
                    return;
                }
                this.stop();
                this._bRunning = true;
                this._iTimer = SchedulingRuntime.restartInterval(this._iTimer, function () {
                    Promise.resolve(this._fnRun()).then(function (oResult) {
                        this.fireEvent(this._sEventName, oResult || {});
                    }.bind(this)).catch(function (oError) {
                        this.fireEvent(this._sErrorEventName, { error: oError });
                    }.bind(this));
                }.bind(this), this._iIntervalMs);
            },

            stop: function () {
                this._bRunning = false;
                this._iTimer = SchedulingRuntime.clearTimer(this._iTimer, clearInterval);
            },

            setIntervalMs: function (iIntervalMs) {
                var iNext = Number(iIntervalMs);
                if (!SchedulingRuntime.isValidDelay(iNext, 1000)) {
                    return;
                }
                this._iIntervalMs = iNext;
                if (this._bRunning) {
                    this.start();
                }
            },

            isRunning: function () {
                return this._bRunning;
            }
        });
    }

    var PollingManager = buildClass("PRODUCTION_CONTROL_CHECKLIST.service.runtime.PollingManager");

    PollingManager.createClass = buildClass;

    return PollingManager;
});
