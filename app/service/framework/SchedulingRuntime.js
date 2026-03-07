sap.ui.define([], function () {
    "use strict";

    function clearTimer(iTimerId) {
        if (iTimerId) {
            window.clearTimeout(iTimerId);
        }
        return 0;
    }

    function restartTimer(iTimerId, fnWork, iDelayMs) {
        clearTimer(iTimerId);
        return window.setTimeout(function () {
            if (typeof fnWork === "function") {
                fnWork();
            }
        }, Number(iDelayMs) || 0);
    }

    function clearFrame(iFrameId) {
        if (iFrameId) {
            window.cancelAnimationFrame(iFrameId);
        }
        return 0;
    }

    function requestFrameOnce(iFrameId, fnWork) {
        if (iFrameId) {
            return iFrameId;
        }
        return window.requestAnimationFrame(function () {
            if (typeof fnWork === "function") {
                fnWork();
            }
        });
    }

    function restartFrame(iFrameId, fnWork) {
        clearFrame(iFrameId);
        return window.requestAnimationFrame(function () {
            if (typeof fnWork === "function") {
                fnWork();
            }
        });
    }

    function nextFrame(fnWork) {
        return window.requestAnimationFrame(function () {
            if (typeof fnWork === "function") {
                fnWork();
            }
        });
    }

    function nextDoubleFrame(fnWork) {
        return nextFrame(function () {
            nextFrame(fnWork);
        });
    }

    return {
        clearTimer: clearTimer,
        restartTimer: restartTimer,
        clearFrame: clearFrame,
        requestFrameOnce: requestFrameOnce,
        restartFrame: restartFrame,
        nextFrame: nextFrame,
        nextDoubleFrame: nextDoubleFrame
    };
});
