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

    _onMatched: function (oEvent) {
      const id = oEvent.getParameter("arguments").id;
      const oComponent = this.getOwnerComponent();
      const oDataModel = oComponent.getModel("data");
      const aChecklists = oDataModel.getProperty("/checkLists") || [];
      const oSelected = aChecklists.find((item) => item.root.id === id) || null;

      oDataModel.setProperty("/selectedChecklist", oSelected);

      const oFcl = oComponent.getRootControl().byId("fcl");
      if (oFcl) {
        oFcl.setLayout("TwoColumnsMidExpanded");
      }
    }

  });
});
