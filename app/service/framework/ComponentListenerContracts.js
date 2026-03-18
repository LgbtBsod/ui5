sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/NavigationContracts"
], function (ModelContracts, NavigationContracts) {
    "use strict";

    return Object.freeze({
        MODEL_NAMES: Object.freeze({
            I18N: ModelContracts.MODELS.I18N,
            LAYOUT: ModelContracts.MODELS.LAYOUT,
            CACHE: "cache",
            MASTER_DATA: ModelContracts.MODELS.MASTER_DATA,
            ENV: "env"
        }),
        PATHS: Object.freeze({
            ACTIVE_OBJECT_ID: "/activeObjectId",
            DETAIL_ACCESS_GUARD: "/detailAccessGuard",
            IS_DIRTY: "/isDirty",
            IS_LOADING: "/isLoading",
            NAV_GUARD_BYPASS: "/navGuardBypass",
            SELECTED_ID: "/selectedId"
        }),
        VALUES: Object.freeze({
            AUTHORIZED: "AUTHORIZED",
            DISCARD: "DISCARD",
            FULL_SAVE_EVENT: "pcct:fullSave",
            LOCK_OWNED: "LOCK_OWNED",
            LOCK_RELEASED: "LOCK_RELEASED",
            NO_CHANGES: "NO_CHANGES",
            ONE_COLUMN: NavigationContracts.LAYOUTS.ONE_COLUMN,
            SAVE: "SAVE",
            UNSAVED_CHANGES_MESSAGE: "You have unsaved changes"
        }),
        TELEMETRY_EVENT: Object.freeze({
            LOCK_STATE_CHANGED: "lock.state.changed",
            WORKFLOW_MODE_CHANGED: "workflow.mode.changed"
        })
    });
});
