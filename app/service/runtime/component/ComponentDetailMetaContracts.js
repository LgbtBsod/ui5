sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowRuntimeConstants"
], function (WorkflowRuntimeConstants) {
    "use strict";

    return Object.freeze({
        READINESS_STATUS: Object.freeze({
            DENIED: WorkflowRuntimeConstants.READINESS_STATUS.DENIED,
            ERROR: WorkflowRuntimeConstants.READINESS_STATUS.ERROR,
            IDLE: WorkflowRuntimeConstants.READINESS_STATUS.IDLE
        }),
        VALIDATION_STATE: Object.freeze({
            IDLE: WorkflowRuntimeConstants.VALIDATION_STATUS.IDLE
        })
    });
});
