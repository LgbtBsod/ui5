sap.ui.define([
    "sap/ui/core/Core"
], function (Core) {
    "use strict";

    function isDarkTheme() {
        var oConfig = Core && Core.getConfiguration && Core.getConfiguration();
        var sTheme = oConfig && oConfig.getTheme && oConfig.getTheme();
        return /dark/i.test(String(sTheme || ""));
    }

    return {
        DETAIL_ACCESS_DENIED_DARK: "assets/illustrations/detail-access-denied-dark.svg",
        DETAIL_ACCESS_DENIED_LIGHT: "assets/illustrations/detail-access-denied-light.svg",
        resolveDetailAccessDeniedIllustration: function () {
            return isDarkTheme()
                ? this.DETAIL_ACCESS_DENIED_DARK
                : this.DETAIL_ACCESS_DENIED_LIGHT;
        }
    };
});
