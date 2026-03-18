sap.ui.define([], function () {
    "use strict";

    function stopManager(oManager, sMethod) {
        var sResolvedMethod = sMethod || "stop";
        if (oManager && typeof oManager[sResolvedMethod] === "function") {
            oManager[sResolvedMethod]();
        }
    }

    function startManager(oManager) {
        if (oManager && typeof oManager.start === "function") {
            oManager.start();
        }
    }

    function isManagerRunning(oManager) {
        return !!(oManager && typeof oManager.isRunning === "function" && oManager.isRunning());
    }

    function resetGcd(oManagers) {
        if (oManagers && oManagers.gcd && typeof oManagers.gcd.resetOnFullSave === "function") {
            oManagers.gcd.resetOnFullSave();
        }
    }

    function stopLockScoped(oManagers) {
        [oManagers && oManagers.heartbeat, oManagers && oManagers.autosave, oManagers && oManagers.lockStatus, oManagers && oManagers.activity, oManagers && oManagers.gcd].forEach(function (oManager) {
            stopManager(oManager);
        });
    }

    function startLockScoped(oManagers) {
        if (!oManagers) {
            return;
        }
        if (!isManagerRunning(oManagers.heartbeat)) {
            startManager(oManagers.heartbeat);
        }
        if (!isManagerRunning(oManagers.autosave)) {
            startManager(oManagers.autosave);
        }
        if (!isManagerRunning(oManagers.lockStatus)) {
            startManager(oManagers.lockStatus);
        }
        if (!isManagerRunning(oManagers.activity)) {
            startManager(oManagers.activity);
        }
        if (!isManagerRunning(oManagers.gcd)) {
            startManager(oManagers.gcd);
            return;
        }
        resetGcd(oManagers);
    }

    function stopAll(oManagers) {
        stopLockScoped(oManagers);
        if (oManagers && oManagers.gcd && typeof oManagers.gcd.destroyManager === "function") {
            oManagers.gcd.destroyManager();
        }
    }

    return {
        resetAfterWrite: resetGcd,
        startLockScoped: startLockScoped,
        stopAll: stopAll,
        stopLockScoped: stopLockScoped
    };
});
