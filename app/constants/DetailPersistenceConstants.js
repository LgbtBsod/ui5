sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowRuntimeConstants"
], function (WorkflowRuntimeConstants) {
    "use strict";

    return Object.freeze({
        STATES: WorkflowRuntimeConstants.PERSISTENCE_STATES,
        TAXONOMY: Object.freeze({
            PERMISSION_DENIED: "PERMISSION_DENIED",
            LOCK_EXPIRED: "LOCK_EXPIRED",
            LOCK_NOT_OWNED_BY_SESSION: "LOCK_NOT_OWNED_BY_SESSION",
            LOCK_STOLEN: "LOCK_STOLEN",
            LOCK_MISSING: "LOCK_MISSING",
            VERSION_CONFLICT: "VERSION_CONFLICT",
            VALIDATION_ERROR: "VALIDATION_ERROR",
            NETWORK_ERROR: "NETWORK_ERROR",
            TECHNICAL_ERROR: "TECHNICAL_ERROR"
        })
    });
});
