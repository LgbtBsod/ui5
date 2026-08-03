sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts"
], function (NavigationContracts) {
    "use strict";

    function normalizeDetailLayout(vLayout) {
        var sLayout = String(vLayout || "").trim();
        var sLower = sLayout.toLowerCase();
        if (sLayout === NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN || sLower === "midcolumnfullscreen") {
            return NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN;
        }
        if (
            sLayout === NavigationContracts.LAYOUTS.TWO_COLUMNS_BEGIN_EXPANDED ||
            sLower === "twocolumnsbeginexpanded"
        ) {
            return NavigationContracts.LAYOUTS.TWO_COLUMNS_BEGIN_EXPANDED;
        }
        if (
            sLayout === NavigationContracts.LAYOUTS.TWO_COLUMNS_MID_EXPANDED ||
            sLower === "twocolumnsmidexpanded"
        ) {
            return NavigationContracts.LAYOUTS.TWO_COLUMNS_MID_EXPANDED;
        }
        return NavigationContracts.LAYOUTS.TWO_COLUMNS_BEGIN_EXPANDED;
    }

    function resolveLayoutFromRoute(sRouteName, mArgs) {
        if (sRouteName === NavigationContracts.ROUTES.SEARCH) {
            return NavigationContracts.LAYOUTS.ONE_COLUMN;
        }
        if (sRouteName === NavigationContracts.ROUTES.ANALYTICS) {
            return NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN;
        }
        if (sRouteName === NavigationContracts.ROUTES.DETAIL) {
            return NavigationContracts.LAYOUTS.TWO_COLUMNS_BEGIN_EXPANDED;
        }
        return null;
    }

    return {
        normalizeDetailLayout: normalizeDetailLayout,
        resolveLayoutFromRoute: resolveLayoutFromRoute
    };
});
