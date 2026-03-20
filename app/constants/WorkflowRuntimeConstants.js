sap.ui.define([], function () {
    "use strict";

    var REQUEST_GROUPS = Object.freeze({
        AUTOSAVE: "autosave",
        CHANGES: "changes",
        LOCKS: "locks",
        SAVE_FLOW: "saveFlow"
    });

    var READINESS_STATUS = Object.freeze({
        DENIED: "denied",
        ERROR: "error",
        IDLE: "idle",
        LOADING: "loading",
        PENDING: "pending",
        READY: "ready"
    });

    var PERSISTENCE_STATES = Object.freeze({
        AUTOSAVING: "autosaving",
        CONFLICT: "conflict",
        DIRTY: "dirty",
        ERROR: "error",
        IDLE: "idle",
        IDLE_TIMEOUT_GRACE: "idleTimeoutGrace",
        LOCK_LOST: "lockLost",
        READ_ONLY: "readOnly",
        SAVED: "saved",
        SAVING: "saving"
    });

    var VALIDATION_STATUS = Object.freeze({
        IDLE: "idle"
    });

    var SOURCES = Object.freeze({
        AUTOSAVE: "autosave",
        ENTER_EDIT: "enter_edit",
        GATEWAY: "gateway",
        IDLE: "idle"
    });

    return Object.freeze({
        PERSISTENCE_STATES: PERSISTENCE_STATES,
        READINESS_STATUS: READINESS_STATUS,
        REQUEST_GROUPS: REQUEST_GROUPS,
        SOURCES: SOURCES,
        VALIDATION_STATUS: VALIDATION_STATUS
    });
});
