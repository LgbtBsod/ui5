sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchStickyLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/SearchUiContracts"
], function (SearchStickyLayoutRuntime, SearchUiContracts) {
    "use strict";
    var VIEWPORT = SearchUiContracts.VIEWPORT;

    return {
        isCompactStickyViewport: function () {
            return typeof window !== "undefined"
                && Number(window.innerWidth || 0) > 0
                && Number(window.innerWidth || 0) <= VIEWPORT.MOBILE_STICKY_BREAKPOINT_PX;
        },
        resolveShellHeaderOffset: function (iMinOffsetPx, iPaddingPx, oScrollHost) {
            return SearchStickyLayoutRuntime.resolveShellHeaderOffsetPx(iMinOffsetPx, iPaddingPx, oScrollHost);
        }
    };
});
