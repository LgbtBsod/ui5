sap.ui.define([
  "sap/ui/core/mvc/Controller"
], function (Controller) {
  "use strict";

  return Controller.extend("theme7.controller.Object", {

    onInit: function () {
      this.getOwnerComponent().getRouter()
        .getRoute("object")
        .attachPatternMatched(this._onMatched, this);
    },

    _onMatched: function (oEvent) {
      var id = oEvent.getParameter("arguments").id;
      var oModel = this.getOwnerComponent().getModel("dataModel");

      if (id === "__create") {
        oModel.setProperty("/object", {});
      } else {
        var mockObj = {
          id: id,
          date: "2025-05-15",
          time: "12:00",
          checks: [
            { no:1, text:"Check A", comment:"", result:"YES" },
            { no:2, text:"Check B", comment:"", result:"NO" }
          ]
        };
        oModel.setProperty("/object", mockObj);
      }

      // Switch to mid column
      var oFCL = this.getOwnerComponent().getRootControl().byId("fcl");
      oFCL.setLayout("TwoColumnsMidExpanded");
    },

    onToggleEdit: function (oEvent) {
      var isEdit = oEvent.getParameter("state");
      this.getOwnerComponent().getModel("stateModel").setProperty("/mode", isEdit ? "EDIT" : "READ");
    }

  });
});