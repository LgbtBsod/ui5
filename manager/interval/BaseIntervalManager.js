sap.ui.define([], function () {
    "use strict";

    function BaseIntervalManager(fnTick) {
        this._fnTick = fnTick;
        this._id = null;
        this._ms = 0;
    }

    BaseIntervalManager.prototype.start = function (ms) {
        this.stop();
        this._ms = Math.max(1000, Number(ms) || 1000);
        this._id = window.setInterval(this._fnTick, this._ms);
    };

    BaseIntervalManager.prototype.stop = function () {
        if (this._id) {
            window.clearInterval(this._id);
            this._id = null;
        }
    };

    BaseIntervalManager.prototype.updateInterval = function (ms) {
        this.start(ms || this._ms);
    };

    return BaseIntervalManager;
});
