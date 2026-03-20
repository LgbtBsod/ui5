sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        STATES: Object.freeze({
            IDLE: "idle",
            DIRTY: "dirty",
            SAVING: "saving",
            AUTOSAVING: "autosaving",
            SAVED: "saved",
            ERROR: "error",
            LOCK_LOST: "lockLost",
            READ_ONLY: "readOnly",
            IDLE_TIMEOUT_GRACE: "idleTimeoutGrace",
            CONFLICT: "conflict"
        }),
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
