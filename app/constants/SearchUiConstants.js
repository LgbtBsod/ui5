sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
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
        })
    });
});
