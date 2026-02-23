sap.ui.define([
  "sap/ui/core/mvc/Controller"
], function (Controller) {
  "use strict";

  return Controller.extend("theme7.controller.App", {

    onToggleTheme: function () {
      document.body.classList.toggle("appDark");
    }

  });
});