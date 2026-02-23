sap.ui.define([
  "sap/ui/core/mvc/Controller"
], function (Controller) {
  "use strict";

  return Controller.extend("sap_ui5.controller.Detail", {

    onInit: function () {
      this.getOwnerComponent().getRouter()
        .getRoute("detail")
        .attachPatternMatched(this._onMatched, this);
    },

    _onMatched: function () {
      this.getView().getParent().setLayout("ThreeColumnsMidExpanded");
    }

  });
});
