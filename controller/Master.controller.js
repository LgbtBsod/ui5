sap.ui.define([
  "sap/ui/core/mvc/Controller"
], function (Controller) {
  "use strict";

  return Controller.extend("sap_ui5.controller.Master", {

    onInit: function () {
      var oData = [
        { id:"1001", date_check:"2025-01-01", lpc_t:"L1", prof_t:"P1" },
        { id:"1002", date_check:"2025-02-15", lpc_t:"L2", prof_t:"P2" }
      ];
      this.getOwnerComponent().getModel("dataModel").setProperty("/searchResults", oData);
    },

    onSelect: function (oEvent) {
      var id = oEvent.getSource().getBindingContext("dataModel").getProperty("id");
      this.getOwnerComponent().getRouter().navTo("object", { id: id });
    },

    onCreate: function () {
      // mock: open empty card
      this.getOwnerComponent().getRouter().navTo("object", { id: "__create" });
    },

    onCopy: function () {
      sap.m.MessageToast.show("Copy item (demo)");
    }

  });
});