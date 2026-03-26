sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        INFO_CARD_KEYS: Object.freeze({
            DATETIME: "datetime",
            EQUIPMENT: "equipment",
            LOCATION: "location",
            LPC: "lpc",
            OBSERVED: "observed",
            OBSERVER: "observer",
            PROFESSION: "profession"
        }),
        INFO_CARD_TEXT_KEYS: Object.freeze({
            DATETIME: "dateTimeBlockLabel",
            EQUIPMENT: "equipmentLabel",
            LOCATION: "locationLabel",
            LPC: "lpcLabel",
            OBSERVED: "observedLabel",
            OBSERVER: "observerLabel",
            PROFESSION: "professionLabel"
        }),
        CARD_REQUIRED_KEYS: Object.freeze({
            datetime: Object.freeze(["basic.date", "basic.time", "basic.timezone"]),
            equipment: Object.freeze(["basic.equipment"]),
            observer: Object.freeze(["basic.OBSERVER_FULLNAME"]),
            observed: Object.freeze(["basic.OBSERVED_FULLNAME"]),
            location: Object.freeze(["basic.LOCATION_KEY"]),
            lpc: Object.freeze(["basic.LPC_KEY"]),
            profession: Object.freeze(["basic.PROF_KEY"])
        }),
        VIEW_DEFAULTS: Object.freeze({
            ATTACHMENT_CATEGORY_KEY: "GEN"
        }),
        ATTACHMENTS: Object.freeze({
            CSS_CLASSES: Object.freeze({
                DROP_ACTIVE: "isAttachmentDropActive",
                DROP_PRIMED: "isAttachmentDropPrimed"
            }),
            DOM_FILE_TYPE_MOZ: "application/x-moz-file",
            DOM_FILE_TYPE_STANDARD: "Files",
            HINT_FALLBACK_EMPTY_TOKEN: "-",
            HINT_FALLBACK_SEPARATOR: " - ",
            HINT_TEXT_KEY: "attachmentUploadHint",
            MIME_REJECTED_TOAST_KEY: "attachmentMimeRejected",
            SIZE_REJECTED_TOAST_KEY: "attachmentSizeRejected",
            UPLOAD_BUSY_PATH: "/attachmentBusy",
            UPLOAD_DISABLED_TOAST_KEY: "attachmentUploadDisabled"
        }),
        STATES: Object.freeze({
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
        }),
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
        MODEL_PATHS: Object.freeze({
            ATTACHMENTS: "/current/attachments",
            BASE: "/base",
            BASE_ATTACHMENTS: "/base/attachments",
            BASE_ROOT_ID: "/base/root/id",
            ROOT: "/current",
            ROOT_ID: "/current/root/id",
            ROOT_INTEGRATION_FLAG: "/current/root/integrationFlag"
        }),
        REASONS: Object.freeze({
            AUTOSAVE_GUARD: "autosaveGuard",
            CREATE_DRAFT_PENDING: "createDraftPending",
            DETAIL_DELETE_COMPLETED: "detailDeleteCompleted",
            DETAIL_SAVE_COMPLETED: "detailSaveCompleted"
        }),
        ATTACHMENT_DELETED: "attachmentDeleted",
        ATTACHMENT_UPLOADED: "attachmentUploaded",
        CHECKLIST_DELETED: "checklistDeleted",
        DELETE_CHECKLIST_CONFIRM: "deleteChecklistConfirmText",
        ATTACHMENT_DRAFT_STAGE_HINT: "attachmentDraftStageHint"
    });
});
