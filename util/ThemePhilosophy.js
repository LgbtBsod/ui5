sap.ui.define([], function () {
  "use strict";

  var THEMES = {
    sap_fiori_3: {
      key: "sap_fiori_3",
      lifestyleClass: "themeLifestyleClarity",
      title: "Clarity Day",
      subtitle: "Focused, airy, analytical"
    },
    sap_fiori_3_dark: {
      key: "sap_fiori_3_dark",
      lifestyleClass: "themeLifestyleNightOps",
      title: "Night Ops",
      subtitle: "Calm, tactical, high-contrast"
    }
  };

  function getMeta(sTheme) {
    return THEMES[sTheme] || THEMES.sap_fiori_3;
  }

  return {
    getMeta: getMeta
  };
});
