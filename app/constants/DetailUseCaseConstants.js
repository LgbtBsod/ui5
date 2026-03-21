sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        ACCESS_OPERATIONS: Object.freeze({
            CREATE: "01",
            CHANGE: "02",
            DISPLAY: "03",
            DELETE: "06"
        }),
        ACCESS_REASON_CODES: Object.freeze({
            AUTHORIZED: "AUTHORIZED",
            CREATE_DRAFT: "CREATE_DRAFT"
        }),
        CODES: Object.freeze({
            AUTOSAVE_EMPTY_DELTA: "AUTOSAVE_EMPTY_DELTA",
            AUTOSAVE_UNAVAILABLE: "AUTOSAVE_UNAVAILABLE",
            CREATE_HANDLER_MISSING: "CREATE_HANDLER_MISSING",
            DELETE_UNAVAILABLE: "DELETE_UNAVAILABLE",
            EXPIRED: "EXPIRED",
            INVALID_INPUT: "INVALID_INPUT",
            LOCK_ACQUIRE_FAILED: "LOCK_ACQUIRE_FAILED",
            LOCKED: "LOCKED",
            LOCKED_OWN_SESSION: "LOCKED_OWN_SESSION",
            KILLED: "KILLED",
            LOCK_REQUIRED: "LOCK_REQUIRED",
            MISSING_CLIENT_VERSION: "MISSING_CLIENT_VERSION",
            NO_CHANGES: "NO_CHANGES",
            NO_CHECKLIST: "NO_CHECKLIST",
            NO_CREATE_PERMISSION: "NO_CREATE_PERMISSION",
            NO_DELETE_PERMISSION: "NO_DELETE_PERMISSION",
            NO_EDIT_PERMISSION: "NO_EDIT_PERMISSION",
            NO_VIEW_PERMISSION: "NO_VIEW_PERMISSION",
            OK: "OK",
            PERMISSION_CHECK_FAILED: "PERMISSION_CHECK_FAILED",
            PERMISSION_CHECK_UNAVAILABLE: "PERMISSION_CHECK_UNAVAILABLE",
            SAVE_HANDLER_MISSING: "SAVE_HANDLER_MISSING",
            TECHNICAL_ERROR: "ERROR"
        }),
        MODEL_PATHS: Object.freeze({
            ATTACHMENTS: "/current/attachments",
            BASE: "/base",
            BASE_ATTACHMENTS: "/base/attachments",
            BASE_ROOT_ID: "/base/root/id",
            ROOT: "/current",
            ROOT_ID: "/current/root/id"
        }),
        REASONS: Object.freeze({
            AUTOSAVE_GUARD: "autosaveGuard",
            CREATE_DRAFT_PENDING: "createDraftPending",
            DETAIL_DELETE_COMPLETED: "detailDeleteCompleted",
            DETAIL_SAVE_COMPLETED: "detailSaveCompleted"
        })
    });
});
