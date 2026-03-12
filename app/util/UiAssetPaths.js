sap.ui.define([], function () {
    "use strict";

    var DETAIL_ACCESS_DENIED_LIGHT = "assets/illustrations/detail-access-denied-light.svg";
    var DETAIL_ACCESS_DENIED_DARK = "assets/illustrations/detail-access-denied-dark.svg";

    function isDarkTheme() {
        var oBody = document && document.body;
        var sTheme = oBody && typeof oBody.getAttribute === "function"
            ? String(oBody.getAttribute("data-theme") || "").trim().toLowerCase()
            : "";
        return sTheme === "dark";
    }

    function resolveDetailAccessDeniedIllustration() {
        return isDarkTheme() ? DETAIL_ACCESS_DENIED_DARK : DETAIL_ACCESS_DENIED_LIGHT;
    }

    return {
        DETAIL_ACCESS_DENIED_LIGHT: DETAIL_ACCESS_DENIED_LIGHT,
        DETAIL_ACCESS_DENIED_DARK: DETAIL_ACCESS_DENIED_DARK,
        resolveDetailAccessDeniedIllustration: resolveDetailAccessDeniedIllustration
    };
});
