sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts"
], function (ModelContracts, NavigationContracts, ModelPathContracts, StatePaths, DetailUseCaseConstants) {
    "use strict";

    return Object.freeze({
        MODEL_NAMES: Object.freeze({
            I18N: ModelContracts.MODELS.I18N,
            SHELL: ModelContracts.MODELS.SHELL,
            MASTER_DATA: ModelContracts.MODELS.MASTER_DATA
        }),
        PATHS: Object.freeze({
            ACTIVE_OBJECT_ID: ModelPathContracts.ACTIVE_OBJECT_ID,
            DETAIL_ACCESS_GUARD: "/detailAccessGuard",
            IS_DIRTY: "/isDirty",
            IS_LOADING: StatePaths.IS_LOADING,
            NAV_GUARD_BYPASS: "/navGuardBypass",
            SELECTED_ID: ModelPathContracts.SELECTED_ID
        }),
        VALUES: Object.freeze({
            AUTHORIZED: "AUTHORIZED",
            DISCARD: "DISCARD",
            FULL_SAVE_EVENT: "pcct:fullSave",
            LOCK_OWNED: "LOCK_OWNED",
            LOCK_RELEASED: "LOCK_RELEASED",
            NO_CHANGES: DetailUseCaseConstants.CODES.NO_CHANGES,
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
