sap.ui.define([
  "sap_ui5/controller/Base.controller",
  "sap_ui5/service/backend/BackendAdapter",
  "sap/m/MessageToast"
], function (BaseController, BackendAdapter, MessageToast) {
  "use strict";

  return BaseController.extend("sap_ui5.controller.Detail", {

    onInit: function () {
      this.attachRouteMatched("detail", this._onMatched);
    },

    _onMatched: function (oEvent) {
      var sId = oEvent.getParameter("arguments").id;
      var sLayout = oEvent.getParameter("arguments").layout || "TwoColumnsMidExpanded";
      var oStateModel = this.getModel("state");
      var oDataModel = this.getModel("data");
      var aChecklists = oDataModel.getProperty("/checkLists") || [];
      var oSelected = aChecklists.find(function (item) {
        return item && item.root && item.root.id === sId;
      }) || null;

      oDataModel.setProperty("/selectedChecklist", oSelected);
      this.getModel("selected").setData(oSelected || {});
      oStateModel.setProperty("/layout", sLayout);
      oStateModel.setProperty("/mode", "READ");
    },

    onCloseDetail: function () {
      this.getModel("state").setProperty("/layout", "OneColumn");
      this.navTo("search", {}, true);
    },

    onToggleEditFromDetail: function (oEvent) {
      var bEditMode = oEvent.getParameter("state");

      if (!bEditMode) {
        this.getModel("state").setProperty("/mode", "READ");
        return;
      }

      this.getModel("state").setProperty("/mode", "EDIT");
    },

    onCancelEditFromDetail: function () {
      var oSelected = this.getModel("data").getProperty("/selectedChecklist") || {};
      this.getModel("selected").setData(JSON.parse(JSON.stringify(oSelected)));
      this.getModel("state").setProperty("/mode", "READ");
    },

    onSaveDetail: function () {
      var oStateModel = this.getModel("state");
      var oDataModel = this.getModel("data");
      var oBundle = this.getResourceBundle();
      var oEdited = this.getModel("selected").getData() || {};
      var sId = ((((oEdited || {}).root || {}).id) || "").trim();

      if (!sId) {
        MessageToast.show("Checklist id not found");
        return;
      }

      oStateModel.setProperty("/isBusy", true);

      BackendAdapter.updateCheckList(sId, oEdited)
        .then(function (oSavedChecklist) {
          return BackendAdapter.getCheckLists().then(function (aUpdatedCheckLists) {
            return {
              savedChecklist: oSavedChecklist,
              checkLists: aUpdatedCheckLists
            };
          });
        })
        .then(function (oResult) {
          oDataModel.setProperty("/checkLists", oResult.checkLists);
          oDataModel.setProperty("/visibleCheckLists", oResult.checkLists);
          oDataModel.setProperty("/selectedChecklist", oResult.savedChecklist);
          this.getModel("selected").setData(oResult.savedChecklist);
          oStateModel.setProperty("/mode", "READ");
          MessageToast.show(oBundle.getText("objectSaved"));
        }.bind(this))
        .catch(function (oError) {
          MessageToast.show(oBundle.getText("objectSaveFailed", [((oError && oError.message) || "Unknown error")]));
        })
        .finally(function () {
          oStateModel.setProperty("/isBusy", false);
        });
    },

    resultText: function (bResult) {
      return bResult ? "Passed" : "Failed";
    },

    resultState: function (bResult) {
      return bResult ? "Success" : "Error";
    }

  });
});
