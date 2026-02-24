sap.ui.define([
  "sap/ui/core/mvc/Controller"
], function (Controller) {
  "use strict";

  return Controller.extend("sap_ui5.controller.App", {});

onToggleTheme: function () {
  const current = sap.ui.getCore().getConfiguration().getTheme();
  const next = current === "sap_fiori_3"
    ? "sap_fiori_3_dark"
    : "sap_fiori_3";

  sap.ui.getCore().applyTheme(next);
  document.body.classList.toggle("appDark");
}

});
