sap.ui.define([
  "sap_ui5/controller/Base.controller"
], function (BaseController) {
  "use strict";

  return BaseController.extend("sap_ui5.controller.Detail", {

    onInit: function () {
      this.attachRouteMatched("detail", this._onMatched);
    },

    _onMatched: function (oEvent) {
      var sId = oEvent.getParameter("arguments").id;
      var sLayout = oEvent.getParameter("arguments").layout || "TwoColumnsMidExpanded";
      var oDataModel = this.getModel("data");
      var aChecklists = oDataModel.getProperty("/checkLists") || [];
      var oSelected = aChecklists.find(function (item) {
        return item && item.root && item.root.id === sId;
      }) || null;

      oDataModel.setProperty("/selectedChecklist", oSelected);
      this.getModel("selected").setData(oSelected || {});
      this.getModel("state").setProperty("/layout", sLayout);
    },

    onCloseDetail: function () {
      this.getModel("state").setProperty("/layout", "OneColumn");
      this.navTo("search", {}, true);
    },


    resultText: function (bResult) {
      return bResult ? "Passed" : "Failed";
    },

    resultState: function (bResult) {
      return bResult ? "Success" : "Error";
    }

  });
});
