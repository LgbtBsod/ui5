sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        DETAIL: Object.freeze({
            VALIDATION_ERROR: "VALIDATION_ERROR",
            TECHNICAL_ERROR: "TECHNICAL_ERROR",
            LOCK_MISSING: "LOCK_MISSING",
            LOCK_EXPIRED: "LOCK_EXPIRED",
            LOCK_STOLEN: "LOCK_STOLEN",
            LOCK_NOT_OWNED_BY_SESSION: "LOCK_NOT_OWNED_BY_SESSION",
            PERMISSION_DENIED: "PERMISSION_DENIED",
            LOCK_OK: "LOCK_OK"
        }),
        FLOW: Object.freeze({
            CREATE_DRAFT: "CREATE_DRAFT",
            LOCKED: "LOCKED",
            LOCKED_OWN_SESSION: "LOCKED_OWN_SESSION",
            OK: "OK",
            SAVED: "SAVED",
            NO_CHANGES: "NO_CHANGES"
        })
    });
});
