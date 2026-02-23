sap.ui.define([
  "sap/ui/core/mvc/Controller"
], function (Controller) {
  "use strict";

  return Controller.extend("sap_ui5.controller.App", {

    onToggleTheme: function () {
      document.body.classList.toggle("appDark");
    }

  });
});