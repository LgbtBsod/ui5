sap.ui.define([], function () {
    "use strict";

    function restartAutoRefresh(mArgs) {
        var iIntervalMs = Number(mArgs.intervalMs) || 15 * 60 * 1000;
        var iTimerId = mArgs.currentTimer || null;

        if (iTimerId) {
            mArgs.clearInterval(iTimerId);
        }

        mArgs.runRefresh();

        iTimerId = mArgs.setInterval(function () {
            mArgs.runRefresh();
        }, iIntervalMs);

        return {
            ok: true,
            timerId: iTimerId,
            intervalMs: iIntervalMs
        };
    }

    function stopAutoRefresh(mArgs) {
        if (mArgs.currentTimer) {
            mArgs.clearInterval(mArgs.currentTimer);
        }
        return {
            ok: true,
            timerId: null
        };
    }

    return {
        restartAutoRefresh: restartAutoRefresh,
        stopAutoRefresh: stopAutoRefresh
    };
});
