sap.ui.define([], function () {
  "use strict";

  var THEMES = {
    sap_fiori_3: {
      lifestyleClass: "themeLifestyleClarity",
      platformClass: "platformCupertinoGlass",
      horizonClass: "themeHorizonMorning",
      designLanguage: "sap-fiori-3"
    },
    sap_fiori_3_dark: {
      lifestyleClass: "themeLifestyleNightOps",
      platformClass: "platformCupertinoGlass",
      horizonClass: "themeHorizonNight",
      designLanguage: "sap-fiori-3-dark"
    },
    sap_horizon: {
      lifestyleClass: "themeLifestyleClarity",
      platformClass: "platformCupertinoGlass",
      horizonClass: "themeHorizonMorning",
      designLanguage: "sap-horizon-morning"
    },
    sap_horizon_dark: {
      lifestyleClass: "themeLifestyleNightOps",
      platformClass: "platformCupertinoGlass",
      horizonClass: "themeHorizonNight",
      designLanguage: "sap-horizon-night"
    }
  };

  function hasTheme(sTheme) {
    return Object.prototype.hasOwnProperty.call(THEMES, sTheme);
  }

  function getMeta(sTheme) {
    return THEMES[sTheme] || THEMES.sap_horizon;
  }

  return {
    hasTheme: hasTheme,
    getMeta: getMeta
  };
});
