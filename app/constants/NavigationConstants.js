sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ShellPaneConstants"
], function (ShellPaneConstants) {
    "use strict";

    var ROUTES = Object.freeze({
        SEARCH: "search",
        ANALYTICS: "analytics",
        DETAIL: "detail",
        DETAIL_LAYOUT: "detailLayout"
    });

    var LAYOUTS = Object.freeze({
        ONE_COLUMN: "OneColumn",
        TWO_COLUMNS_BEGIN_EXPANDED: "TwoColumnsBeginExpanded",
        TWO_COLUMNS_MID_EXPANDED: "TwoColumnsMidExpanded",
        MID_COLUMN_FULL_SCREEN: "MidColumnFullScreen"
    });

    var MID_COLUMN_PAGE_IDS = Object.freeze({
        ANALYTICS: ShellPaneConstants.PAGE_IDS[ShellPaneConstants.PANES.ANALYTICS],
        DETAIL: ShellPaneConstants.PAGE_IDS[ShellPaneConstants.PANES.DETAIL]
    });

    function isDetailRoute(sRouteName) {
        return sRouteName === ROUTES.DETAIL || sRouteName === ROUTES.DETAIL_LAYOUT;
    }

    function resolveMidColumnPageId(sRouteName) {
        return sRouteName === ROUTES.ANALYTICS ? MID_COLUMN_PAGE_IDS.ANALYTICS : MID_COLUMN_PAGE_IDS.DETAIL;
    }

    return Object.freeze({
        ROUTES: ROUTES,
        LAYOUTS: LAYOUTS,
        MID_COLUMN_PAGE_IDS: MID_COLUMN_PAGE_IDS,
        isDetailRoute: isDetailRoute,
        resolveMidColumnPageId: resolveMidColumnPageId
    });
});
