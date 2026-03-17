sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/schema/uiSchema",
    "PRODUCTION_CONTROL_CHECKLIST/model/schema/workflowSchema",
    "PRODUCTION_CONTROL_CHECKLIST/model/schema/navigationSchema"
], function (uiSchema, workflowSchema, navigationSchema) {
    "use strict";

    function createTimers() {
        return {
            heartbeatMs: 270000, lockStatusMs: 60000, gcdMs: 300000, idleMs: 570000,
            autoSaveIntervalMs: 150000, autoSaveDebounceMs: 1200, networkGraceMs: 60000,
            lockRefreshCooldownMs: 150000, analyticsRefreshMs: 900000,
            retryBaseDelayMs: 500, retryMaxDelayMs: 10000, cacheToleranceMs: 5500
        };
    }

    function createStateDefaults() {
        return Object.assign({}, navigationSchema, uiSchema, workflowSchema, { timers: createTimers() });
    }

    return {
        createTimers: createTimers,
        createStateDefaults: createStateDefaults
    };
});
