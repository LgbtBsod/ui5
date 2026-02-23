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

    _onMatched: function () {
      this.getView().getParent().setLayout("TwoColumnsMidExpanded");
    },

    onToggleEdit: function (oEvent) {
      var on = oEvent.getParameter("state");
      var stateModel = this.getOwnerComponent().getModel("stateModel");

      stateModel.setProperty("/mode", on ? "EDIT" : "READ");

      if (on) {
        document.body.classList.add("editMode");
      } else {
        document.body.classList.remove("editMode");
      }
    }

  });
});