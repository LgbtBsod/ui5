sap.ui.define([], function () {
  "use strict";

  var THEMES = {
    sap_fiori_3: {
      lifestyleClass: "themeLifestyleClarity"
    },
    sap_fiori_3_dark: {
      lifestyleClass: "themeLifestyleNightOps"
    },
    sap_horizon: {
      lifestyleClass: "themeLifestyleClarity"
    },
    sap_horizon_dark: {
      lifestyleClass: "themeLifestyleNightOps"
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
