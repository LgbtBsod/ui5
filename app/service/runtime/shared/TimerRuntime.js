sap.ui.define([], function () {
    "use strict";

    function isValidDelay(iDelayMs, iMinMs) {
        var iValue = Number(iDelayMs);
        var iMin = Number(iMinMs || 0);
        return Number.isFinite(iValue) && iValue >= iMin;
    }

    function clearTimer(iTimer, fnClear) {
        if (iTimer) {
            fnClear(iTimer);
        }
        return null;
    }

    function restartTimeout(iTimer, fnWork, iDelayMs) {
        clearTimer(iTimer, clearTimeout);
        return setTimeout(fnWork, iDelayMs);
    }

    function restartInterval(iTimer, fnWork, iDelayMs) {
        clearTimer(iTimer, clearInterval);
        return setInterval(fnWork, iDelayMs);
    }

    return {
        clearTimer: clearTimer,
        isValidDelay: isValidDelay,
        restartInterval: restartInterval,
        restartTimeout: restartTimeout
    };
});
