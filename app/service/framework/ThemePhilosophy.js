sap.ui.define([], function () {
    "use strict";

    var THEMES = {
        sap_fiori_3: {
            lifestyleClass: "themeLifestyleClarity",
            platformClass: "platformPrecisionEnterprise",
            horizonClass: "themeHorizonMorning",
            designLanguage: "sap-fiori-3"
        },
        sap_fiori_3_dark: {
            lifestyleClass: "themeLifestyleNightOps",
            platformClass: "platformCalmModern",
            horizonClass: "themeHorizonNight",
            designLanguage: "sap-fiori-3-dark"
        }
    };

    function hasTheme(sTheme) {
        return Object.prototype.hasOwnProperty.call(THEMES, sTheme);
    }

    function getMeta(sTheme) {
        return THEMES[sTheme] || THEMES.sap_fiori_3;
    }

    return {
        getMeta: getMeta,
        hasTheme: hasTheme
    };
});
