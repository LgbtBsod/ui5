sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/schema/uiSchema",
    "PRODUCTION_CONTROL_CHECKLIST/model/schema/workflowSchema",
    "PRODUCTION_CONTROL_CHECKLIST/model/schema/navigationSchema"
], function (uiSchema, workflowSchema, navigationSchema) {
    "use strict";

    function createTimers() {
        return {
            heartbeatMs: 240000, lockStatusMs: 60000, gcdMs: 300000, idleMs: 600000,
            autoSaveIntervalMs: 60000, autoSaveDebounceMs: 1200, networkGraceMs: 60000,
            cacheFreshMs: 30000, cacheStaleOkMs: 90000, analyticsRefreshMs: 300000,
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
