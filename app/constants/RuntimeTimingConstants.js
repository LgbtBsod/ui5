sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        STATE_TIMERS: Object.freeze({
            heartbeatMs: 270000,
            lockStatusMs: 60000,
            gcdMs: 300000,
            idleMs: 570000,
            autoSaveIntervalMs: 150000,
            autoSaveDebounceMs: 1200,
            networkGraceMs: 60000,
            lockRefreshCooldownMs: 150000,
            analyticsRefreshMs: 900000,
            retryBaseDelayMs: 500,
            retryMaxDelayMs: 10000,
            cacheToleranceMs: 5500
        })
    });
});
