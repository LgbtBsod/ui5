sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/schema/uiSchema",
    "PRODUCTION_CONTROL_CHECKLIST/model/schema/workflowSchema",
    "PRODUCTION_CONTROL_CHECKLIST/model/schema/navigationSchema",
    "PRODUCTION_CONTROL_CHECKLIST/constants/RuntimeOrchestrationContracts"
], function (uiSchema, workflowSchema, navigationSchema, RuntimeTimingConstants) {
    "use strict";

    function createTimers() {
        return Object.assign({}, RuntimeTimingConstants.STATE_TIMERS);
    }

    function createStateDefaults() {
        return Object.assign({}, navigationSchema, uiSchema, workflowSchema, { timers: createTimers() });
    }

    return {
        createTimers: createTimers,
        createStateDefaults: createStateDefaults
    };
});
