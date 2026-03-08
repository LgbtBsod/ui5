sap.ui.define([], function () {
    "use strict";

    return {
        UI_BUSY_GLOBAL: "/isLoading",
        UI_BUSY_SEARCH_TABLE: "/isBusy",
        UI_BUSY_DETAIL: "/isBusy",
        UI_BUSY_ATTACHMENTS: "/isBusy",


        CONTRACT_UI_BUSY_GLOBAL: "/ui/busy/global",
        CONTRACT_UI_BUSY_SEARCH_TABLE: "/ui/busy/searchTable",
        CONTRACT_UI_BUSY_DETAIL: "/ui/busy/detail",
        CONTRACT_WORKFLOW_DETAIL_EDIT_MODE: "/workflow/detail/editMode",
        CONTRACT_WORKFLOW_DETAIL_LOCK_STATE: "/workflow/detail/lock/state",
        CONTRACT_WORKFLOW_DETAIL_AUTOSAVE_STATE: "/workflow/detail/autosave/state",
        CONTRACT_WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT: "/workflow/detail/autosave/lastSavedAt",
        CONTRACT_WORKFLOW_SEARCH_MODE: "/workflow/search/mode",
        CONTRACT_WORKFLOW_SEARCH_SEGMENTS: "/workflow/search/segments",

        UI_FEEDBACK_INLINE_ERRORS: "/inlineErrors",
        UI_FEEDBACK_CONFLICT_DIALOG: "/conflictDialog",

        WORKFLOW_SEARCH_MODE: "/searchMode",
        WORKFLOW_SEARCH_SEGMENTS: "/search",
        WORKFLOW_SEARCH_SEGMENTS_CHECKS: "/search/checksFailSegment",
        WORKFLOW_SEARCH_SEGMENTS_BARRIERS: "/search/barriersFailSegment",

        SESSION_ID: "/sessionId",
        SAVE_IN_FLIGHT: "/saveInFlight",
        PENDING_NAVIGATION_INTENT: "/pendingNavigationIntent",
        VALIDATION_SUMMARY: "/validationSummary",
        TAB_CONFLICT_STATE: "/tabConflictState",
        WORKFLOW_EDIT_MODE: "/mode",
        WORKFLOW_LOCK_STATUS: "/lockOperationState",
        WORKFLOW_LOCK_LOST_REASON: "/lockLostReason",
        WORKFLOW_AUTOSAVE_ENABLED: "/autosaveEnabled",
        WORKFLOW_DIRTY: "/isDirty",

        WORKFLOW_DETAIL_EDIT_MODE: "/mode",
        WORKFLOW_DETAIL_LOCK_STATE: "/lockOperationState",
        WORKFLOW_DETAIL_AUTOSAVE_STATE: "/autosaveState",
        WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT: "/autosaveAt"
    };
});
