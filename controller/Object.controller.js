sap.ui.define([
  "sap/ui/core/mvc/Controller"
], function (Controller) {
  "use strict";

  return Controller.extend("sap_ui5.controller.Object", {

    onInit: function () {
      this.getOwnerComponent().getRouter()
        .getRoute("object")
        .attachPatternMatched(this._onMatched, this);
    },

    _onMatched: function (oEvent) {
      var sId = oEvent.getParameter("arguments").id;

      this.getOwnerComponent().getModel("stateModel").setProperty("/selectedId", sId);
      this.getView().getParent().setLayout("TwoColumnsMidExpanded");
    },

    onOpenDetail: function () {
      var sId = this.getOwnerComponent().getModel("stateModel").getProperty("/selectedId") || "1001";

      this.getOwnerComponent().getRouter().navTo("detail", { id: sId });
    },

    onToggleEdit: function (oEvent) {
      var bOn = oEvent.getParameter("state");
      var oStateModel = this.getOwnerComponent().getModel("stateModel");

      oStateModel.setProperty("/mode", bOn ? "EDIT" : "READ");

      if (bOn) {
        document.body.classList.add("editMode");
      } else {
        document.body.classList.remove("editMode");
      }
    }

  });
});
