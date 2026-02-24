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
      const sLayout = oEvent.getParameter("arguments").layout || "TwoColumnsMidExpanded";
      const oComponent = this.getOwnerComponent();
      const oDataModel = oComponent.getModel("data");
      const aChecklists = oDataModel.getProperty("/checkLists") || [];
      const oSelected = aChecklists.find((item) => item.root.id === id) || null;

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

    statusState: function (sStatus) {
      switch (sStatus) {
        case "SUCCESS": return "Success";
        case "WARNING": return "Warning";
        case "CRITICAL": return "Error";
        default: return "None";
      }
    }

  });
});
