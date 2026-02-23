sap.ui.define([
  "sap/ui/core/mvc/Controller"
], function (Controller) {
  "use strict";

  return Controller.extend("theme7.controller.Master", {

    onSelect: function (oEvent) {
      var oItem = oEvent.getParameter("listItem");
      var sId = oItem.getBindingContext("dataModel").getProperty("id");

      this.getOwnerComponent().getRouter().navTo("object", { id: sId });
    }

  });
});
