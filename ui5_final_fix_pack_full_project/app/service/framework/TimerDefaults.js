sap.ui.define([], function () {
    "use strict";

    return {
        heartbeatMs: { min: 500, max: 300000, allowZero: false, defaultValue: 270000, secKeys: ["HeartbeatIntervalSec"] },
        lockStatusMs: { min: 500, max: 300000, allowZero: false, defaultValue: 60000, secKeys: ["StatusPollIntervalSec", "LockPollingIntervalSec"] },
        idleMs: { min: 10000, max: 86400000, allowZero: false, defaultValue: 570000, secKeys: ["IdleTimeoutSec"] },
        autoSaveDebounceMs: { min: 100, max: 60000, allowZero: false, defaultValue: 1200, msKeys: ["AutoSaveDebounceMs"] },
        autoSaveIntervalMs: { min: 1000, max: 3600000, allowZero: false, defaultValue: 150000, msKeys: ["AutoSaveIntervalMs"], secKeys: ["AutoSaveIntervalSec"] },
        lockRefreshCooldownMs: { min: 1000, max: 3600000, allowZero: false, defaultValue: 150000, msKeys: ["LockRefreshCooldownMs"], secKeys: ["LockRefreshCooldownSec"] },
        retryBaseDelayMs: { min: 100, max: 60000, allowZero: false, defaultValue: 500, msKeys: ["RetryBaseDelayMs"] },
        retryMaxDelayMs: { min: 500, max: 3600000, allowZero: false, defaultValue: 10000, msKeys: ["RetryMaxDelayMs"] },
        bootstrapRetryMs: { min: 10, max: 5000, allowZero: false, defaultValue: 50, msKeys: ["BootstrapRetryMs"] },
        cacheToleranceMs: { min: 0, max: 3600000, allowZero: true, defaultValue: 5500, msKeys: ["CacheToleranceMs"] },
        gcdMs: { min: 1000, max: 3600000, allowZero: false, defaultValue: 300000, msKeys: ["GcdIntervalMs"], secKeys: ["GcdIntervalSec"] },
        networkGraceMs: { min: 1000, max: 3600000, allowZero: false, defaultValue: 60000, msKeys: ["NetworkGraceMs"], secKeys: ["NetworkGraceSec"] },
        analyticsRefreshMs: { min: 1000, max: 3600000, allowZero: false, defaultValue: 900000, msKeys: ["AnalyticsRefreshMs"], secKeys: ["AnalyticsRefreshSec"] }
    };
});
