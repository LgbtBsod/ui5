sap.ui.define([], function () {
    "use strict";

    return {
        COLUMN_RULES: {
        Id: { width: "8.5rem", minScreenWidth: "Phone", demandPopin: false, importance: "High" },
            LpcText: { width: "6.75rem", minScreenWidth: "Tablet", demandPopin: true, importance: "High" },
            ProfessionText: { width: "10rem", minScreenWidth: "Desktop", demandPopin: true, importance: "Medium" },
            LocationKey: { width: "9.75rem", minScreenWidth: "Desktop", demandPopin: true, importance: "Medium" },
            Status: { width: "7rem", minScreenWidth: "Tablet", demandPopin: true, importance: "High" },
            SuccessChecksRate: { width: "8rem", minScreenWidth: "Desktop", demandPopin: true, importance: "Medium" },
            SuccessBarriersRate: { width: "8rem", minScreenWidth: "Desktop", demandPopin: true, importance: "Medium" },
            DateCheck: { width: "8rem", minScreenWidth: "Tablet", demandPopin: true, importance: "High" },
            EquipName: { width: "9rem", minScreenWidth: "Desktop", demandPopin: true, importance: "Low" },
            ChangedOn: { width: "9rem", minScreenWidth: "Desktop", demandPopin: true, importance: "Low" }
        },
        SHORTCUT_ACTIONS: {
            CREATE: "create",
            COPY: "copy",
            SELECT_VISIBLE: "selectVisible",
            CLEAR_SELECTION: "clearSelection",
            SEARCH: "search",
            EXPORT: "export",
            FOCUS_FILTERS: "focusFilters",
            FOCUS_RESULTS: "focusResults",
            FOCUS_TOOLBAR: "focusToolbar"
        },
        VIEWPORT: {
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
        }
    };
});
