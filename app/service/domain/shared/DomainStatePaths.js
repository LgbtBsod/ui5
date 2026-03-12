sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts"
], function (StatePaths, ModelPathContracts) {
    "use strict";

    // Deprecated compatibility surface. New code must consume
    // model paths via service/domain/shared/ModelPathContracts
    // and shared state paths via model/StatePaths.
    return Object.freeze({
        ACTIVE_OBJECT_ID: ModelPathContracts.ACTIVE_OBJECT_ID,
        SELECTED_ID: ModelPathContracts.SELECTED_ID,
        CURRENT_ROUTE_NAME: ModelPathContracts.CURRENT_ROUTE_NAME,
        POST_OPEN_HYDRATED_ROOT_ID: ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID,
        ANALYTICS_RETURN_RESTORE_EDIT: ModelPathContracts.ANALYTICS_RETURN_RESTORE_EDIT,
        AUTOSAVE_ENABLED: ModelPathContracts.AUTOSAVE_ENABLED,
        IS_DIRTY: ModelPathContracts.IS_DIRTY,
        LOCK_OPERATION_PENDING: ModelPathContracts.LOCK_OPERATION_PENDING,
        LAYOUT: ModelPathContracts.LAYOUT,
        SEARCH_FORCE_REFRESH_ON_RETURN: ModelPathContracts.SEARCH_FORCE_REFRESH_ON_RETURN,
        LOCK_EXPIRES: ModelPathContracts.LOCK_EXPIRES,
        REQUIRED_FIELDS: ModelPathContracts.REQUIRED_FIELDS,
        SESSION_ID: ModelPathContracts.SESSION_ID,
        UI_BUSY_GLOBAL: StatePaths.UI_BUSY_GLOBAL,
        UI_BUSY_SEARCH_TABLE: StatePaths.UI_BUSY_SEARCH_TABLE,
        UI_BUSY_DETAIL: StatePaths.UI_BUSY_DETAIL,
        WORKFLOW_DETAIL_EDIT_MODE: StatePaths.WORKFLOW_DETAIL_EDIT_MODE,
        WORKFLOW_DETAIL_LOCK_STATE: StatePaths.WORKFLOW_DETAIL_LOCK_STATE,
        WORKFLOW_DETAIL_AUTOSAVE_STATE: StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE,
        WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT: StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT,
        WORKFLOW_SEARCH_MODE: StatePaths.WORKFLOW_SEARCH_MODE,
        WORKFLOW_SEARCH_SEGMENTS: StatePaths.WORKFLOW_SEARCH_SEGMENTS
    });
});
