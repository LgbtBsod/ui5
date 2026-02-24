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
      const sId = oEvent.getParameter("arguments").id;
      const sLayout = oEvent.getParameter("arguments").layout || "TwoColumnsMidExpanded";
      const oComponent = this.getOwnerComponent();
      const oDataModel = oComponent.getModel("data");
      const aChecklists = oDataModel.getProperty("/checkLists") || [];
      const oSelected = aChecklists.find(function (item) {
        return item && item.root && item.root.id === sId;
      }) || null;

      oDataModel.setProperty("/selectedChecklist", oSelected);
      oComponent.getModel("selected").setData(oSelected || {});
      oComponent.getModel("state").setProperty("/layout", sLayout);
    },

    onCloseDetail: function () {
      const oComponent = this.getOwnerComponent();
      oComponent.getModel("state").setProperty("/layout", "OneColumn");
      oComponent.getRouter().navTo("search", {}, true);
    },

    onToggleTheme: function () {
      const oConfig = sap.ui.getCore().getConfiguration();
      const sCurrentTheme = oConfig.getTheme();
      const sNextTheme = sCurrentTheme === "sap_fiori_3" ? "sap_fiori_3_dark" : "sap_fiori_3";

      sap.ui.getCore().applyTheme(sNextTheme);
      document.body.classList.toggle("appDark", sNextTheme === "sap_fiori_3_dark");
    },

    resultText: function (bResult) {
      return bResult ? "Passed" : "Failed";
    },

    resultState: function (bResult) {
      return bResult ? "Success" : "Error";
    }

  });
});
