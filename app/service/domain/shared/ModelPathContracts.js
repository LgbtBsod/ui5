sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (StatePaths) {
    "use strict";

    return Object.freeze({
        ACTIVE_OBJECT_ID: "/activeObjectId",
        ANALYTICS_RETURN_RESTORE_EDIT: "/analyticsReturnRestoreEdit",
        AUTOSAVE_ENABLED: "/autosaveEnabled",
        BACKEND_MODE: "/backendMode",
        CURRENT_ROUTE_NAME: "/currentRouteName",
        IS_DIRTY: "/isDirty",
        LAYOUT: "/layout",
        LOCK_EXPIRES: "/lockExpires",
        LOCK_OPERATION_PENDING: "/lockOperationPending",
        POST_OPEN_HYDRATED_ROOT_ID: "/postOpenHydratedRootId",
        REQUIRED_FIELDS: "/requiredFields",
        SEARCH_FORCE_REFRESH_ON_RETURN: "/searchForceRefreshOnReturn",
        SELECTED_ID: "/selectedId",
        SESSION_ID: StatePaths.SESSION_ID,
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
