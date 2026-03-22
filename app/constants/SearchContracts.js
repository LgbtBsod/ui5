sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        DEFAULTS: Object.freeze({
            SEARCH_BACKEND_TOP: "100",
            SEARCH_VISIBLE_ROWS: "100"
        }),
        PERSISTENCY_PREFIXES: Object.freeze({
            SMART_FILTER_SESSION: "pcctSmartFilterSession_",
            SMART_TABLE_SESSION: "pcctSmartTableSession_"
        }),
        SEARCH_SEGMENTS: Object.freeze({
            ALL: "ALL",
            FAILED: "FAILED",
            SUCCESS: "SUCCESS"
        }),
        SEARCH_MODE: Object.freeze({
            EXACT: "EXACT",
            LOOSE: "LOOSE"
        }),
        COLUMN_RULES: Object.freeze({
            Id: Object.freeze({ width: "8.5rem", minScreenWidth: "Phone", demandPopin: false, importance: "High" }),
            LpcText: Object.freeze({ width: "6.75rem", minScreenWidth: "Tablet", demandPopin: true, importance: "High" }),
            ProfessionText: Object.freeze({ width: "10rem", minScreenWidth: "Desktop", demandPopin: true, importance: "Medium" }),
            LocationKey: Object.freeze({ width: "9.75rem", minScreenWidth: "Desktop", demandPopin: true, importance: "Medium" }),
            Status: Object.freeze({ width: "7rem", minScreenWidth: "Tablet", demandPopin: true, importance: "High" }),
            SuccessChecksRate: Object.freeze({ width: "8rem", minScreenWidth: "Desktop", demandPopin: true, importance: "Medium" }),
            SuccessBarriersRate: Object.freeze({ width: "8rem", minScreenWidth: "Desktop", demandPopin: true, importance: "Medium" }),
            DateCheck: Object.freeze({ width: "8rem", minScreenWidth: "Tablet", demandPopin: true, importance: "High" }),
            EquipName: Object.freeze({ width: "9rem", minScreenWidth: "Desktop", demandPopin: true, importance: "Low" }),
            ChangedOn: Object.freeze({ width: "9rem", minScreenWidth: "Desktop", demandPopin: true, importance: "Low" })
        }),
        SHORTCUT_ACTIONS: Object.freeze({
            CREATE: "create",
            COPY: "copy",
            SELECT_VISIBLE: "selectVisible",
            CLEAR_SELECTION: "clearSelection",
            SEARCH: "search",
            EXPORT: "export",
            FOCUS_FILTERS: "focusFilters",
            FOCUS_RESULTS: "focusResults",
            FOCUS_TOOLBAR: "focusToolbar"
        }),
        VIEWPORT: Object.freeze({
            COMPACT_REM_MAX: 45,
            MOBILE_STICKY_BREAKPOINT_PX: 700,
            LAYOUT_DEBOUNCE_MS: 96,
            STICKY_STACK_GAP_PX: 6,
            SUMMARY_RAIL_GAP_PX: 6,
            MIN_HEADER_OFFSET_PX: 8,
            HEADER_OFFSET_PADDING_PX: 2,
            ANCHOR_SCROLL_MARGIN_PX: 10,
            SCROLL_NAV_TOP_PX: 220,
            RESULTS_NAV_EXTRA_PX: 120,
            POST_ANCHOR_SYNC_DELAY_MS: 280
        }),
        GROUP_DATE_CHECK: "searchGroupDateCheck",
        GROUP_LPC: "searchGroupLpc",
        GROUP_NONE: "searchGroupNone",
        GROUP_PROFESSION: "searchGroupProfession",
        GROUP_STATUS: "searchGroupStatus",
        GROUP_DIALOG_TITLE: "searchGroupDialogTitle",
        NOTHING_TO_OPEN: "nothingToOpen",
        OPEN_USES_FIRST_HINT: "searchOpenUsesFirstHint",
        COPY_SINGLE_SELECTION_HINT: "searchCopySingleSelectionHint",
        RESULTS_LABEL: "resultsLabel",
        SEARCH_MODE_EXACT: "searchModeExact",
        SEARCH_MODE_LABEL: "searchModeLabel",
        SEARCH_MODE_LOOSE: "searchModeLoose",
        SEARCH_SELECTION_NONE: "searchSelectionNone",
        SEARCH_SELECTION_PRIMARY_PREFIX: "searchSelectionPrimaryPrefix",
        SEARCH_SELECTION_UNITS: "searchSelectionUnits",
        SELECT_VISIBLE_EMPTY: "searchSelectVisibleEmpty",
        SORT_CHANGED_ON: "searchSortChangedOn",
        SORT_DATE_CHECK: "searchSortDateCheck",
        SORT_ID: "searchSortId",
        SORT_LPC: "searchSortLpc",
        SORT_PROFESSION: "searchSortProfession",
        SORT_STATUS: "searchSortStatus",
        SORT_DIALOG_TITLE: "searchSortDialogTitle",
        WORKFLOW_STAGE_ANALYZE: "workflowStageAnalyze",
        WORKFLOW_STAGE_DISCOVER: "workflowStageDiscover",
        WORKFLOW_STAGE_REVIEW: "workflowStageReview"
    });
});
