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
            INVALID_INPUT: "INVALID_INPUT",
            LOCK_REQUIRED: "LOCK_REQUIRED",
            MISSING_CLIENT_VERSION: "MISSING_CLIENT_VERSION",
            NO_CHANGES: "NO_CHANGES",
            NO_CHECKLIST: "NO_CHECKLIST",
            NO_CREATE_PERMISSION: "NO_CREATE_PERMISSION",
            NO_DELETE_PERMISSION: "NO_DELETE_PERMISSION",
            NO_EDIT_PERMISSION: "NO_EDIT_PERMISSION",
            NO_VIEW_PERMISSION: "NO_VIEW_PERMISSION",
            PERMISSION_CHECK_FAILED: "PERMISSION_CHECK_FAILED",
            PERMISSION_CHECK_UNAVAILABLE: "PERMISSION_CHECK_UNAVAILABLE",
            SAVE_HANDLER_MISSING: "SAVE_HANDLER_MISSING"
        }),
        MESSAGE_KEYS: Object.freeze({
            ATTACHMENT_DELETED: "attachmentDeleted",
            ATTACHMENT_UPLOADED: "attachmentUploaded",
            CHECKLIST_DELETED: "checklistDeleted",
            DETAIL_CREATE_PERMISSION_DENIED: "detailCreatePermissionDenied",
            DETAIL_DELETE_PERMISSION_DENIED: "detailDeletePermissionDenied",
            DETAIL_VIEW_PERMISSION_DENIED: "detailViewPermissionDenied",
            LOCK_RELEASE_FAILED: "lockReleaseFailed",
            NOTHING_TO_DELETE: "nothingToDelete",
            OBJECT_SAVED: "objectSaved",
            PERSISTENCE_AUTOSAVE_PENDING_ATTACHMENTS: "persistenceAutosavePendingAttachments",
            PERSISTENCE_AUTOSAVE_SAVED: "persistenceAutosaveSaved",
            PERSISTENCE_FORCED_READ_ONLY: "persistenceForcedReadOnly",
            PERSISTENCE_IDLE_TIMEOUT_GRACE: "persistenceIdleTimeoutGrace",
            PERSISTENCE_LOCK_LOST: "persistenceLockLost",
            PERSISTENCE_NO_CHANGES: "persistenceNoChanges"
        }),
        MODEL_PATHS: Object.freeze({
            ATTACHMENTS: "/attachments",
            ROOT: "/",
            ROOT_ID: "/root/id"
        }),
        REASONS: Object.freeze({
            AUTOSAVE_GUARD: "autosaveGuard",
            CREATE_DRAFT_PENDING: "createDraftPending",
            DETAIL_DELETE_COMPLETED: "detailDeleteCompleted",
            DETAIL_SAVE_COMPLETED: "detailSaveCompleted"
        })
    });
});
