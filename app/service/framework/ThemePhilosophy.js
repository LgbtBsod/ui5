sap.ui.define([], function () {
    "use strict";

    var BASE_THEME = "sap_fiori_3";
    var PLATFORM_CONTRACT = "platformPrecisionEnterprise";

    function resolveTheme() {
        return BASE_THEME;
    }

    function resolvePlatformContract() {
        return PLATFORM_CONTRACT;
    }

    return {
        BASE_THEME: BASE_THEME,
        PLATFORM_CONTRACT: PLATFORM_CONTRACT,
        resolveTheme: resolveTheme,
        resolvePlatformContract: resolvePlatformContract
    };
});
