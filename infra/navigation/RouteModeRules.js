sap.ui.define([], function () {
    "use strict";

    function normalizeDetailLayout(vLayout) {
        var sLayout = String(vLayout || "").trim();
        var sLower = sLayout.toLowerCase();
        if (sLayout === "MidColumnFullScreen" || sLower === "midcolumnfullscreen") {
            return "MidColumnFullScreen";
        }
        if (
            sLayout === "TwoColumnsBeginExpanded" ||
            sLower === "twocolumnsbeginexpanded" ||
            sLayout === "TwoColumnsMidExpanded" ||
            sLower === "twocolumnsmidexpanded"
        ) {
            return "TwoColumnsMidExpanded";
        }
        return "TwoColumnsMidExpanded";
    }

    function resolveLayoutFromRoute(sRouteName, mArgs) {
        if (sRouteName === "search") {
            return "OneColumn";
        }
        if (sRouteName === "detail") {
            return "TwoColumnsMidExpanded";
        }
        if (sRouteName === "detailLayout") {
            return normalizeDetailLayout(mArgs && mArgs.layout);
        }
        return null;
    }

    return {
        normalizeDetailLayout: normalizeDetailLayout,
        resolveLayoutFromRoute: resolveLayoutFromRoute
    };
});
